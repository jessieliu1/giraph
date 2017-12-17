open Ast
open Sast
open Exception

module StringMap = Map.Make(String)

type env = {
  env_name : string; (* name of fn*)
  env_return_type : typ; (* return type *)
  env_fmap : fdecl StringMap.t; (* function map *)
  env_sfmap : sfdecl StringMap.t; 
  env_globals : typ StringMap.t; (* global vars *)
  env_flocals : typ StringMap.t; (* function locals *)
  env_fformals : typ StringMap.t; (* function formals *)
  env_in_loop : bool;
  (* todo: built in methods? *)
}

let rec convert_expr e env = match e with 
    Id(str)                       -> (check_id str env, env)
  | Binop(e1, op, e2)             -> (check_binop e1 op e2 env, env) 
  | Unop(op, e)                   -> (check_unop op e env, env)
  | Assign(str, e)                -> (check_assign str e env)
  | Method(e, "data", e_lst)     -> (check_data e e_lst env, env)
  | Method (e, "set_data", e_lst) -> (check_sdata e e_lst env, env)
  | Method (e, s2, e_lst)        -> (report_meth_not_found s2) (*TODO more methods? will all methods be pre-named? *)
  | Call("print", e_lst)          -> (check_print e_lst env, env)
  | Call("printb", e_lst)         -> (check_print e_lst env, env)
  | Call("prints", e_lst)         -> (check_print e_lst env, env)
  | Call(str, e_lst)              -> (check_call str e_lst env, env)
  | Bool_Lit(b)                   -> (SBool_Lit(b), env)
  | Int_Lit(i)                    -> (SInt_Lit(i), env)
  | Float_Lit(f)                  -> (SFloat_Lit(f), env)
  | String_Lit(str)               -> (SString_Lit(str), env)
  (* todo below *)
  | Graph_Lit(str_lst, ed_lst, n_lst, is_d, is_w) ->
    (check_graph str_lst ed_lst n_lst is_d is_w env)
  | Noexpr                        -> (SNoexpr, env)


and convert_expr_list expr_lst env = 
    let sexpr_env_lst = List.map (fun expr -> convert_expr expr env) expr_lst in
    let sexpr_lst = List.map (fun (sexpr, _) -> sexpr) sexpr_env_lst in
    sexpr_lst, env


and get_sexpr_type sexpr = match sexpr with
    SId(_, typ)                 -> typ
  | SBinop(_, _, _, typ)        -> typ
  | SUnop(_, _, typ)            -> typ
  | SAssign(_, _, typ)          -> typ
  | SCall(_, _, typ)            -> typ
  | SMethod(_,_,_, typ)         -> typ
  | SBool_Lit(_)                -> Bool
  | SInt_Lit(_)                 -> Int
  | SFloat_Lit(_)               -> Float
  | SString_Lit(_)              -> String
  | SGraph_Lit(_,_,_,subtype,_) -> subtype
  | SNoexpr                     -> Void


and get_sexpr_lst_type sexpr_lst = 
    List.map (fun sexpr -> get_sexpr_type sexpr) sexpr_lst


(* s is the id being checked *)
and check_id str env =
    try let typ = StringMap.find str env.env_flocals in
        SId(str, typ) with Not_found ->
    (try let typ = StringMap.find str env.env_fformals in
        SId(str, typ) with Not_found ->
    (try let typ = StringMap.find str env.env_globals in
        SId(str, typ) with Not_found ->
                raise (Failure("undeclared identifier " ^ str))))


and check_binop e1 op e2 env =
    let (s1, _) = convert_expr e1 env 
    and (s2, _) = convert_expr e2 env in
    let t1 = get_sexpr_type s1 and t2 = get_sexpr_type s2 in
    (* TODO add checking if types are graph!! *)
    match op with
        Add ->
            (match t1, t2 with
                Int, Int        -> SBinop(s1, op, s2, Int)
                | Float, Float  -> SBinop(s1, op, s2, Float)
                (*TODO: string concat? *)
                | _             -> report_bad_binop t1 op t2
            )
        | Sub | Mult | Div 
            when t1 = t2 && (t1 = Int || t1 = Float) -> SBinop(s1, op, s2, t1)
        | Mod 
            when t1 = Int && t2 = Int -> SBinop(s1, op, s2, t1)
        | Eq | Neq ->
            (match t1, t2 with
                Int, Int        -> SBinop(s1, op, s2, Bool)
                | Bool, Bool    -> SBinop(s1, op, s2, Bool)
                | _             -> report_bad_binop t1 op t2
                 (* TODO: add string compare? *)
            )
        | Less | Leq | Greater | Geq
            when t1 = t2 && (t1 = Int || t1 = Float) -> SBinop(s1, op, s2, Bool)
        | And | Or 
            when t1 = Bool && t2 = Bool -> SBinop(s1, op, s2, Bool) 
                | _             -> report_bad_binop t1 op t2


and check_unop op e env = 
    let (s, _) = convert_expr e env in
    let t = get_sexpr_type s in
    match op with 
        Neg when (t = Int || t = Float) -> SUnop(op, s, t)
        | Not when t = Bool             -> SUnop(op, s, t)
        | _ -> report_bad_unop op t


and check_print e_lst env = 
    if ((List.length e_lst) != 1) then raise(Failure("wrong number of arguments"))
    else
        let (s, _) = convert_expr (List.hd e_lst) env in 
        let t = get_sexpr_type s in
        let strcall = 
            match t with 
                Int         -> "print"
                | String    -> "prints"
                | Bool      -> "printb"
                | _         -> 
                    raise(Failure("type " ^ string_of_typ t ^ " is unsupported for this function"))
        in
        SCall(strcall, [s], t)


and check_assign str e env = 
    let (r, env) = convert_expr e env in
    let rvaluet = get_sexpr_type r in

    (* check if the id has been declared *)
    if StringMap.mem str env.env_flocals || StringMap.mem str env.env_fformals || 
        StringMap.mem str env.env_globals then
        let lvaluet = try StringMap.find str env.env_flocals with
            Not_found -> (try StringMap.find str env.env_fformals with 
                Not_found -> StringMap.find str env.env_globals) in
        (* if types match *)
        if lvaluet == rvaluet then SAssign(str, r, lvaluet), env
        (* TODO: check if rvalue is an edgeless graph and lvaluet is a graph subtype (this should pass) *)
        else report_bad_assign lvaluet rvaluet
    else report_undeclared_id_assign str

and check_data e e_lst env =
    let len = List.length e_lst in
    if (len != 0) then (raise(Failure("data takes 0 arguments but " ^ string_of_int len ^ " arguments given")))
    else 
    let (s,_) = convert_expr e env in
    let t = get_sexpr_type s in
    if (t != Node) then (raise(Failure("data() called on type " ^ string_of_typ t ^ " when node was expected")))
    else SMethod(e,"data", [], Int)  (*TODO get actual type, not Int, once graph data types are in*)



and check_sdata e e_lst env =
        let len = List.length e_lst in
        if (len != 1) then raise(Failure("set_data() takes 1 arguments but " ^ string_of_int len ^ " arguments given"))
        else 
        let (id,_) = convert_expr e env in
        let t = get_sexpr_type id in 
        if (t != Node) then raise(Failure("set_data() called on type " ^ string_of_typ t ^ " when node was expected"))
        else
        let (s, _) = convert_expr (List.hd e_lst) env in 
        SMethod(e,"set_data", [s], Int) (*TODO get actual type, not Int, once graph data types are in*)

(* TODO *)
and check_graph str_lst ed_lst n_lst is_d is_w env =
  let graph_type = match (is_d, is_w) with
      (true, true) -> Wedigraph
    | (true, false) -> Digraph
    | (false, true) -> Wegraph
    | (false, false) -> Graph
  in
  (* convert each weight expression *)
  let ed_lst_checked = List.map (fun (f,t,w) -> (f, t, fst (convert_expr w env))) ed_lst in
  let weight_types = List.map (fun (_,_,w_sexpr) -> get_sexpr_type w_sexpr) ed_lst_checked in
  List.iter (fun t -> if t != Int then
                raise (Failure("edge weights must be of type int"))) weight_types;
  (* TODO: remove all this when generics are implemented *)
  (* first elt must be int *)
  (* match first elt to other elts *)
  match str_lst with
    [] -> SGraph_Lit([], [], [], Graph, Int), env
  | _ -> (match n_lst with
        [] ->  SGraph_Lit(str_lst, ed_lst_checked, [], graph_type, Int), env
      | _ ->
        let (s,_) = convert_expr (snd (List.hd n_lst)) env in
        let t = get_sexpr_type s in
        List.iter (fun n -> let (sn,_) = convert_expr (snd n) env in
                    let tn = get_sexpr_type sn in
                    if tn != t then raise (Failure("node type mismatch of " ^ string_of_typ t ^ " and " ^ string_of_typ tn))) n_lst;

        (*(check_vdecl t str e env)*)
        let newenv = List.fold_left (fun x y -> let (_, z) = check_vdecl Node y Noexpr x in z) env str_lst
        in
        let nodes = List.map (fun (x,y) -> let (s,_) = convert_expr y newenv in (x,s)) n_lst
        in
        (SGraph_Lit(str_lst, ed_lst_checked, nodes, graph_type, Int), newenv))

(* if elt is already defined don't declare, just add to node list*)
(* if elt is not defined declare and assign expr to it *)

and check_call str e_lst env = 
    (* can't call main *)
    if str == "main" then raise (Failure ("cant make call to main"))
    (* check if function can be found*)
    else if not (StringMap.mem str env.env_fmap) then report_function_not_found str
    else 
    (* semantically check all the arguments*)
    let checked_args, env = convert_expr_list e_lst env in 

    (* get the types of the args*)
    let arg_types = get_sexpr_lst_type (checked_args) in

    if not (StringMap.mem str env.env_sfmap) then
        let fdecl = StringMap.find str env.env_fmap in
        let env = 
        {
            env_name = env.env_name;
            env_return_type = env.env_return_type;
            env_fmap = env.env_fmap;
            env_sfmap = (convert_fdecl fdecl.f_name fdecl.f_formals env).env_sfmap;
            env_globals = env.env_globals;
            env_flocals = env.env_flocals;
            env_fformals = env.env_fformals;
            env_in_loop = env.env_in_loop;
        } in
        let sfdecl = StringMap.find str env.env_sfmap in
          try 
              (* confirm types match *)
              List.iter2 (fun t1 (t2, _) -> if t1 != t2 then report_typ_args t2 t1 else ()) arg_types sfdecl.sf_formals;
              let sexpr_lst, _ = convert_expr_list e_lst env in 
                  SCall(str, sexpr_lst, sfdecl.sf_typ)
          with 
          (* wrong number of arguments *)
              Invalid_argument _ -> raise (Failure ("expected " ^ string_of_int (List.length sfdecl.sf_formals) ^ "arguments when " 
              ^ string_of_int (List.length e_lst) ^ " arguments were provided"))
    else
          let sfdecl = StringMap.find str env.env_sfmap in
          try 
              (* confirm types match *)
              List.iter2 (fun t1 (t2, _) -> if t1 != t2 then report_typ_args t2 t1 else ()) arg_types sfdecl.sf_formals;
              let sexpr_lst, _ = convert_expr_list e_lst env in 
                  SCall(str, sexpr_lst, sfdecl.sf_typ)
          with 
          (* wrong number of arguments *)
              Invalid_argument _ -> raise (Failure ("expected " ^ string_of_int (List.length sfdecl.sf_formals) ^ "arguments when " 
              ^ string_of_int (List.length e_lst) ^ " arguments were provided"))

and convert_stmt stmt env = match stmt with 
    Block(s_lst)            -> (check_block s_lst env, env) 
    | If(e, s1, s2)         -> (check_if e s1 s2 env, env)
    | For(e1, e2, e3, s)    -> (check_for e1 e2 e3 s env, env) 
    | While(e, s)           -> (check_while e s env, env)
    | For_Node(str, e, s)   -> (check_for_node str e s env, env)
    | For_Edge(str, e, s)   -> (check_for_edge str e s env, env)
    | Bfs(str, e1, e2, s)   -> (check_bfs str e1 e2 s env, env)
    | Dfs(str, e1, e2, s)   -> (check_dfs str e1 e2 s env, env) 
    | Break                 -> (check_break env, env)  
    | Continue              -> (check_continue env, env) 
    | Expr(e)               -> (check_expr_stmt e env) 
    | Vdecl(t, str, e)      -> (check_vdecl t str e env)
    | Return(e)             -> (check_return e env, env)


and check_block s_lst env = 
    match s_lst with 
    []      -> SBlock([SExpr(SNoexpr, Void)]) 
    | _ -> (*check every statement, and put those checked statements in a list*)
              let rec add_sstmt acc stmt_lst env = match stmt_lst with
                [] -> acc
                | Return _ :: _ :: _  -> raise (Failure("nothing may follow a return"))
                | st :: st_lst -> 
                      let sstmt, new_env = convert_stmt st env in 
                      let new_acc = sstmt::acc in add_sstmt new_acc st_lst new_env
              in
              let sblock = add_sstmt [] s_lst env in
            SBlock(sblock)


and check_if cond is es env =
    (* semantically check the condition *)
    let scond,env = convert_expr cond env in 
        (match get_sexpr_type scond with 
            Bool ->
                let (sis, _) = convert_stmt is env in
                let (ses, _) = convert_stmt es env in
                SIf(scond, sis, ses)
            | _ -> raise(Failure("Expected boolean expression")))    


and check_for e1 e2 e3 s env = 
    let (se1, env1) = convert_expr e1 env in (* a new var may be added to locals there*)
    let (se2, env2) = convert_expr e2 env1 in
    let (se3, env3) = convert_expr e3 env2 in
    let new_env = 

    {
      env_name = env.env_name;
      env_return_type = env3.env_return_type;
      env_fmap = env3.env_fmap;
      env_sfmap = env3.env_sfmap;
      env_globals = env3.env_globals;
      env_flocals = env3.env_flocals;
      env_fformals = env3.env_fformals;
      env_in_loop = true;
    }
    in

    let (for_body, _) = convert_stmt s new_env in

    if (get_sexpr_type se2) = Bool then
        SFor(se1, se2, se3, for_body)
    else raise(Failure("Expected boolean expression"))


and check_while e s env =
    let (se, _) = convert_expr e env in
    let new_env = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = true;
    }
    in

    let (while_body, _) = convert_stmt s new_env in
    if (get_sexpr_type se) = Bool then
        SWhile(se, while_body)
    else raise(Failure("Expected boolean expression"))

(* for_node (neighbor : residual.get_neighbors(n)) *)
and check_for_node str e s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    let (se, senv) = convert_expr e new_env in 
    let (for_body, _) = convert_stmt s senv in
    SFor_Node(str, se, for_body)

and check_for_edge str e s env = 
    let flocals = StringMap.add str Edge env.env_flocals in
    let new_env = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    let (se, senv) = convert_expr e new_env in
    let (for_body, _) = convert_stmt s senv in
    SFor_Edge(str, se, for_body)


and check_bfs str e1 e2 s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    let (se1, senv1) = convert_expr e1 new_env in 
    let (se2, senv2) = convert_expr e2 senv1 in
    let (bfs_body, _) = convert_stmt s senv2 in 
    SBfs(str, se1, se2, bfs_body)


and check_dfs str e1 e2 s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    let (se1, senv1) = convert_expr e1 new_env in 
    let (se2, senv2) = convert_expr e2 senv1 in
    let (dfs_body, _) = convert_stmt s senv2 in 
    SDfs(str, se1, se2, dfs_body)


and check_break env = 
    if env.env_in_loop then
        SBreak
    else raise(Failure("can't break outside of a loop"))


and check_continue env = 
    if env.env_in_loop then
        SContinue
    else raise(Failure("can't continue outside of a loop"))


and check_expr_stmt e env = 
    let (se, nenv) = convert_expr e env in 
    let typ = get_sexpr_type se in (SExpr(se, typ), nenv)


and convert_fdecl fname fformals env = 
    let fdecl = StringMap.find fname env.env_fmap 
    in

    report_duplicate (fun n -> match n with 
                _, str -> "duplicate fformal " ^ str) fformals;

    let formals_to_map m formal = 
      match formal with
      (t, str) -> StringMap.add str t m
    in
    let formals = List.fold_left formals_to_map StringMap.empty fformals 
    in 

    let env = {
        env_name = fname;
        env_return_type = fdecl.f_typ;
        env_fmap = env.env_fmap;
        env_sfmap = env.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = StringMap.empty; (* locals should be empty at start check*)
        env_fformals = formals;
        env_in_loop = env.env_in_loop
    }
    in 

    (* Semantically check all statements of the body *)
    let (sstmts, env) = convert_stmt (Block fdecl.f_body) env
    in 

    let sfdecl = { 
        sf_typ = env.env_return_type;
        sf_name = fdecl.f_name;
        sf_formals = fformals; (*skips check? *)
        sf_body = match sstmts with SBlock(sl) -> sl | _ -> [] ;
    }
    in
    let new_env = { 
        env_name = fname;
        env_return_type = fdecl.f_typ;
        env_fmap = env.env_fmap;
        env_sfmap = StringMap.add fname sfdecl env.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = env.env_flocals;
        env_fformals = formals;
        env_in_loop = env.env_in_loop
    } 
  in new_env
    

and check_vdecl t str e env = 
    if (StringMap.mem str env.env_flocals || StringMap.mem str env.env_fformals || StringMap.mem str env.env_globals)
    then raise(Failure("cannot reinitialize existing variable"))
    else
    let flocals = StringMap.add str t env.env_flocals in
    let new_env = 
      {
        env_name = env.env_name;
        env_return_type = env.env_return_type;
        env_fmap = env.env_fmap;
        env_sfmap = env.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = flocals;
        env_fformals = env.env_fformals;
        env_in_loop = env.env_in_loop;
      }
    in

    let (se, nenv) = convert_expr e new_env 
    in
    let typ = get_sexpr_type se 
    in
    if typ != Void then
      (if t != typ then (raise(Failure("expression type mismatch " ^ string_of_typ t ^ " and " ^ string_of_typ typ)))
      else (SVdecl(t, str, se), nenv))
    else 
      (SVdecl(t, str, se), nenv)


and check_return e env =
    let se, env = convert_expr e env in 
    let typ = get_sexpr_type se in
    if typ = env.env_return_type 
    then SReturn(se)
    else raise(Failure("expected return type " ^ string_of_typ env.env_return_type 
      ^ " but got return type " ^ string_of_typ typ))

let convert_ast globals fdecls fmap = 
    let _ = try StringMap.find "main" fmap with
        Not_found -> raise(Failure("missing main")) 
    in

    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

    let convert_globals m global = 
      match global with
      (typ, str) -> if (typ != Void) then StringMap.add str typ m
      else raise(Failure("global " ^ str ^ " cannot have a void type"))
    in

    (* semantically checked globals in a map *)
    let globals_map = List.fold_left convert_globals StringMap.empty globals
    in 

    let globals_lst = List.rev(List.fold_left (fun acc binding -> match binding with (sfname, _) -> sfname::acc) [] (StringMap.bindings globals_map))
    in 

    (* check for duplicate functions *)
    report_duplicate (fun f -> "duplicate function " ^ f.f_name) fdecls;
    
    let env = {
        env_name = "main";
        env_return_type = Int;
        env_fmap = fmap;
        env_sfmap = StringMap.empty;
        env_globals = globals_map;
        env_flocals = StringMap.empty;
        env_fformals = StringMap.empty;
        env_in_loop = false;
    }
    in

    (* this is the environment with all the sfdecls, stemming from main *)
    let sfdecl_env = convert_fdecl "main" [] env in 

    let sfdecls = List.rev(List.fold_left (fun lst (_, sfdecl) -> sfdecl :: lst)
                  [] (StringMap.bindings sfdecl_env.env_sfmap)) 
    in (globals_lst, sfdecls)


let build_fmap fdecls = 
    (* built in *)
    let built_in_decls =  StringMap.add "print"
     { f_typ = Void; f_name = "print"; f_formals = [(Int, "x")];
       f_body = [] } (StringMap.add "printb"
     { f_typ = Void; f_name = "printb"; f_formals = [(Bool, "x")];
       f_body = [] } (StringMap.add "prints"
     { f_typ = Void; f_name = "prints"; f_formals = [(String, "x")];
       f_body = [] } (StringMap.singleton "printbig"
     { f_typ = Void; f_name = "printbig"; f_formals = [(Int, "x")];
       f_body = [] })))
    in 

    let check_fdecls map fdecl = 
        if StringMap.mem fdecl.f_name map then
            raise (Failure ("duplicate function " ^ fdecl.f_name))
        else if StringMap.mem fdecl.f_name built_in_decls then
            raise (Failure ("reserved function name " ^ fdecl.f_name))
        else StringMap.add fdecl.f_name fdecl map
    in
    List.fold_left (fun map fdecl -> check_fdecls map fdecl) built_in_decls fdecls


let check ast = match ast with
    (globals, fdecls) ->
      let fmap = build_fmap fdecls in
      let sast = convert_ast globals fdecls fmap
      in 
      sast 
