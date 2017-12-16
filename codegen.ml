module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Giraph"

  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and str_t = L.pointer_type (L.i8_type context)
  and float_t = L.float_type  context
  and void_t = L.void_type context
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and i32_ptr_t = L.pointer_type (L.i32_type context) in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.String -> str_t
    | A.Node -> i32_ptr_t
    | A.Graph -> void_ptr_t
    | A.Edge -> i32_t
    | A.Void -> void_t in
  (* TODO: actually add all types *)

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare functions that will be called to construct graphs *)
  let new_graph_t = L.function_type void_ptr_t [||] in
  let new_graph_func = L.declare_function "new_graph" new_graph_t the_module in

  let add_vertex_t = L.function_type void_ptr_t [| void_ptr_t ; i32_ptr_t |] in
  let add_vertex_func = L.declare_function "add_vertex" add_vertex_t the_module in

  let add_edge_t = L.function_type void_t [| void_ptr_t ; void_ptr_t |] in
  let add_edge_func = L.declare_function "add_edge" add_edge_t the_module in

  let new_data_t = L.function_type i32_ptr_t [||] in
  let new_data_func = L.declare_function "new_data" new_data_t the_module in

  let set_data_t = L.function_type void_t [| i32_ptr_t ; i32_t |] in
  let set_data_func = L.declare_function "set_data" set_data_t the_module in

  let get_data_t = L.function_type i32_t [| i32_ptr_t |] in
  let get_data_func = L.declare_function "get_data" get_data_t the_module in

  (* Declare functions that will be called to iterate through graphs *)
  let num_vertices_t = L.function_type i32_t [| void_ptr_t |] in
  let num_vertices_func = L.declare_function "num_vertices" num_vertices_t the_module in

  let get_head_vertex_t = L.function_type void_ptr_t [| void_ptr_t |] in
  let get_head_vertex_func = L.declare_function "get_head_vertex" get_head_vertex_t the_module in

  let get_next_vertex_t = L.function_type void_ptr_t [| void_ptr_t |] in
  let get_next_vertex_func = L.declare_function "get_next_vertex" get_next_vertex_t the_module in

  let get_data_from_vertex_t = L.function_type i32_ptr_t [| void_ptr_t |] in
  let get_data_from_vertex_func = L.declare_function "get_data_from_vertex" get_data_from_vertex_t the_module in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.f_name
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.f_formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.f_typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.f_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder and
      string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Allocate formal arguments on the stack, initialize their value, and
       remember their values in a map also containing all global vars. *)
    let globals_and_formals =
      let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      in
      List.fold_left2 add_formal global_vars fdecl.A.f_formals
        (Array.to_list (L.params the_function))
    in

    (* Construct the locally declared variables for a block. Allocate each on the
       stack, initialize their value, if appropriate, and remember their values in
       the map m, passed as an argument. This is called every time A.Block is processed,
       allowing every bracketed block to have its own scope. *)
    let add_local_vars m builder sl =
      (* When initializing graphs with graph literals, nodes do not have to be
         declared; e.g. "graph g = [A -- B]" implicitly declares nodes A and B (unless
         they have been already declared explicitly or in a previous graph). Thus,
         whenever we encounter a graph literal, we have to construct all the new nodes
         it uses as local variables. The following function handles this. *)
      let add_nodes_from_graph_lits m expr = match expr with
          A.Assign(id, e) -> (match e with
            A.Graph_Lit(nodes, edges, _) ->
            let add_node m node =
              if (StringMap.mem node m) then
                m
              else
                let local_node_var = L.build_alloca (ltype_of_typ A.Node) node builder in
                let new_data_ptr = L.build_call new_data_func [||] "tmp_data" builder in
                ignore(L.build_store new_data_ptr local_node_var builder);
                StringMap.add node local_node_var m
            in
            List.fold_left add_node m nodes
            | _ -> m (* ignore non-graph expressions *))
        | _ -> m (* ignore non-assign statements -
                    TODO: figure out if graph literals could appear anywhere else *)
      in

      (* find all local variables declared in block; ignore other statements *)
      let add_local m stmt = match stmt with
          A.Vdecl(t, n, e) ->
          (* if e contains a graph literal, adds new nodes to m; else m unchanged *)
          let m = add_nodes_from_graph_lits m e in
          let local_var = L.build_alloca (ltype_of_typ t) n builder in
          (* if we're declaring a node, we need to call new_data() from C to get a unique
             data pointer and store it in the allocated register *)
          (match t with
             A.Node ->
             let new_data_ptr = L.build_call new_data_func [||] "tmp_data" builder in
             ignore(L.build_store new_data_ptr local_var builder);
           | _ -> ());
          (* add new variable to m *)
          StringMap.add n local_var m
        | A.Expr(e) -> add_nodes_from_graph_lits m e
        | _ -> m
      in
      List.fold_left add_local m sl (* return value of add_local_vars *)
    in

    (* Given a symbol table "vars", return the value for a variable
       or formal argument in the table *)
    let lookup vars n = StringMap.find n vars in

    (* Construct code for an expression; return its value *)
    let rec expr vars builder = function
        A.Int_Lit i -> L.const_int i32_t i
      | A.Bool_Lit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup vars s) s builder
      | A.String_Lit s -> L.build_global_stringptr s "str" builder
      | A.Float_Lit f -> L.const_float float_t f
      | A.Binop (e1, op, e2) ->
        let e1' = expr vars builder e1
        and e2' = expr vars builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.Mod     -> L.build_srem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Eq      -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq     -> L.build_icmp L.Icmp.Sge) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
        let e' = expr vars builder e in
        (match op with
           A.Neg     -> L.build_neg
         | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign(id, e) -> let e' = expr vars builder e in
        ignore (L.build_store e' (lookup vars id) builder); e'
      | A.Graph_Lit (nodes, edges, nodes_init) ->
        (* create new graph struct, return pointer *)
        let g = L.build_call new_graph_func [||] "tmp" builder in
        (* map node names to vertex_list_node pointers created by calling add_vertex *)
        let get_data_ptr node = L.build_load (lookup vars node) node builder in
        let call_add_vertex node = L.build_call add_vertex_func [| g ; (get_data_ptr node) |] ("vertex_struct_" ^ node) builder in
        let nodes_map = List.fold_left (fun map node -> StringMap.add node (call_add_vertex node) map) StringMap.empty nodes in
        (* add edges in both directions *)
        ignore(List.map (fun (n1, n2) -> L.build_call add_edge_func [| (StringMap.find n1 nodes_map) ; (StringMap.find n2 nodes_map) |] "" builder) edges);
        ignore(List.map (fun (n1, n2) -> L.build_call add_edge_func [| (StringMap.find n2 nodes_map) ; (StringMap.find n1 nodes_map) |] "" builder) edges);
        (* initialize nodes with data *)
        let set_data (node, data) = L.build_call set_data_func [| (get_data_ptr node) ; (expr vars builder data) |] "" builder in
        ignore(List.map set_data nodes_init);
        (* return pointer to graph struct *)
        g
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
        L.build_call printf_func [| int_format_str ; (expr vars builder e) |]
          "printf" builder
      | A.Call ("prints", [e]) ->
        L.build_call printf_func [| string_format_str ; (expr vars builder e) |]
          "prints" builder
      | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (expr vars builder) (List.rev act)) in
        let result = (match fdecl.A.f_typ with A.Void -> ""
                                             | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder

      (* node methods *)
      | A.Method (node_expr, "data", []) ->
        let data_ptr = expr vars builder node_expr in
        L.build_call get_data_func [| data_ptr |] "tmp_data" builder
      | A.Method (node_expr, "set_data", [data]) ->
        let data_ptr = expr vars builder node_expr in
        L.build_call set_data_func [| data_ptr ; (expr vars builder data) |] "" builder

      (* graph methods *)
      | A.Method (graph_expr, "add_node", [node_expr]) ->
        let graph_ptr = expr vars builder graph_expr
        and data_ptr = expr vars builder node_expr in
        L.build_call add_vertex_func [| graph_ptr ; data_ptr |] "tmp" builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt vars builder = function
        A.Block sl -> let vars = add_local_vars vars builder sl in
        List.fold_left (stmt vars) builder sl
      | A.Expr e -> ignore (expr vars builder e); builder
      | A.Vdecl(t, n, e) -> ignore (expr vars builder e); builder
      | A.Return e -> ignore (match fdecl.A.f_typ with
            A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr vars builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr vars builder predicate in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt vars (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt vars (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt vars (L.builder_at_end context body_bb) body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr vars pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
      | A.Break -> builder (*not implemented *)
      | A.Continue -> builder (*not implemented *)
      | A.For (e1, e2, e3, body) ->
        stmt vars builder( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
      | A.For_Node (n, g, body) ->
        let graph_ptr = (expr vars builder g) in

        (* allocate counter variable - counts number of nodes seen so far *)
        let counter = L.build_alloca i32_t "counter" builder in
        ignore(L.build_store (L.const_int i32_t 0) counter builder);
        (* get number of nodes in graph *)
        let size = L.build_call num_vertices_func [| graph_ptr |] "size" builder in
        (* allocate register for n; then, add to symbol table, so the body can access it *)
        let node_var = L.build_alloca (ltype_of_typ A.Node) n builder in
        let vars = StringMap.add n node_var vars in
        (* allocate pointer to current vertex struct *)
        let current_vertex_ptr = L.build_alloca void_ptr_t "current" builder in
        (* get head of vertex list *)
        let head_vertex = L.build_call get_head_vertex_func [| graph_ptr |] "head" builder in
        ignore(L.build_store head_vertex current_vertex_ptr builder);

        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let body_builder = L.builder_at_end context body_bb in
        (* load value of current vertex *)
        let current_vertex = L.build_load current_vertex_ptr "current_tmp" body_builder in
        (* get node data pointer from current vertex struct *)
        let data_ptr = L.build_call get_data_from_vertex_func [| current_vertex |] (n ^ "_tmp") body_builder in
        ignore(L.build_store data_ptr node_var body_builder);
        (* change current_vertex to be pointer to next_vertex *)
        let next_vertex = L.build_call get_next_vertex_func [| current_vertex |] "next" body_builder in
        ignore(L.build_store next_vertex current_vertex_ptr body_builder);
        (* increment counter *)
        let counter_val = L.build_load counter "counter_tmp" body_builder in
        let counter_incr = L.build_add (L.const_int i32_t 1) counter_val "counter_incr" body_builder in
        ignore(L.build_store counter_incr counter body_builder);
        (* build body of loop *)
        add_terminal (stmt vars body_builder body) (L.build_br pred_bb);

        (* branch to while_body iff counter < size *)
        let pred_builder = L.builder_at_end context pred_bb in
        let counter_val = L.build_load counter "counter_tmp" pred_builder in
        let done_bool_val = L.build_icmp L.Icmp.Slt counter_val size "done" pred_builder in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br done_bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb

      | A.For_Edge (e1, e2, e3) -> builder (*not implemented *)
      | A.Bfs (e1, e2, e3, s) -> builder (*not implemented *)
      | A.Dfs (e1, e2, e3, s) -> builder (*not implemented*)
    in

    (* Build the code for each statement in the function *)
    let builder = stmt globals_and_formals builder (A.Block fdecl.A.f_body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.f_typ with
          A.Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
