(* Authors: 
Daniel Benett deb2174
Seth Benjamin sjb2190
Jennifer Bi jb3495
Jessie Liu jll2219
*)
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
  | Method (e, "from", e_lst)     -> (check_edgemtd e "from" e_lst env)
  | Method (e, "to", e_lst)     -> (check_edgemtd e "to" e_lst env)
  | Method (e, "weight", e_lst)   -> (check_edgemtd e "weight" e_lst env)
  | Method (e, "set_weight", e_lst)   -> (check_edgemtd e "set_weight" e_lst env)
  | Method(e, "data", e_lst)     -> (check_data e e_lst env)
  | Method (e, "set_data", e_lst) -> (check_sdata e e_lst env)
  | Method (e, "print", e_lst) -> (check_graphmtd e "print" 0 e_lst Void env)
  | Method (e, "add_node", e_lst) -> (check_graphmtd e "add_node" 1 e_lst Void env)
  | Method (e, "remove_node", e_lst) -> (check_graphmtd e "remove_node" 1 e_lst Void env)
  | Method (e, "has_node", e_lst) -> (check_graphmtd e "has_node" 1 e_lst Bool env)
  | Method (e, "add_edge", [f;t]) -> (check_graphmtd e "add_edge" 2 [f;t] Void env)
  | Method (e, "add_edge", [f;t;w]) -> (check_graphmtd e "add_edge" 3 [f;t;w] Void  env)
  | Method (e, "remove_edge", e_lst) -> (check_graphmtd e "remove_edge" 2 e_lst Void env)
  | Method (e, "has_edge", e_lst) -> (check_graphmtd e "has_edge" 2 e_lst Bool env)
  | Method (e, "neighbors", e_lst) -> (check_graphmtd e "neighbors" 1 e_lst (Graph(Int)) env)
  | Method (e, "get_edge_weight", e_lst) -> (check_graphmtd e "get_edge_weight" 2 e_lst Int env)
  | Method (e, "set_edge_weight", e_lst) -> (check_graphmtd e "set_edge_weight" 3 e_lst Int env)
  | Method (e, "put", e_lst) -> (check_mapmtd e "put" 2 e_lst env)
  | Method (e, "get", e_lst) -> (check_mapmtd e "get" 1 e_lst env)
  | Method (e, "contains", e_lst) -> (check_mapmtd e "contains" 1 e_lst env)
  | Method (e, s2, e_lst)        -> (report_meth_not_found s2)
  | Call("print", e_lst)          -> (check_print e_lst env)
  | Call("printb", e_lst)         -> (check_print e_lst env)
  | Call("prints", e_lst)         -> (check_print e_lst env)
  | Call(str, e_lst)              -> (check_call str e_lst env)
  | Int_Lit(i)                    -> (SInt_Lit(i), env)
  | Float_Lit(f)                  -> (SFloat_Lit(f), env)
  | String_Lit(str)               -> (SString_Lit(str), env)
  | Bool_Lit(bool)                -> (SBool_Lit(bool), env)
  | Graph_Lit(str_lst, ed_lst, n_lst, is_d, is_w) ->
    (check_graph str_lst ed_lst n_lst is_d is_w env)
  | Noexpr                        -> (SNoexpr, env)


and convert_expr_list expr_lst env = 
    match expr_lst with
    [] -> [], env
    | _ -> 
          let rec add_sexpr acc ex_lst env = match ex_lst with
            [] -> acc, env
            | e :: e_lst -> 
                  let sexpr, new_env = convert_expr e env in 
                  let new_acc = sexpr::acc in add_sexpr new_acc e_lst new_env
          in
          let sexpr_lst, nenv = add_sexpr [] expr_lst env in
          let new_env = {
            env_name = env.env_name;
            env_return_type = env.env_return_type;
            env_fmap = env.env_fmap;
            env_sfmap = nenv.env_sfmap;
            env_globals = env.env_globals;
            env_flocals = env.env_flocals;
            env_fformals = env.env_fformals;
            env_in_loop = env.env_in_loop;
            }
    in
    List.rev sexpr_lst, new_env


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

and check_id_typ str typ env =
    if not (StringMap.mem str env.env_flocals || StringMap.mem str env.env_fformals 
            || StringMap.mem str env.env_globals)
    then raise(Failure("node " ^ str ^ " is undefined"))
    else
      let lval = try StringMap.find str env.env_flocals with
          Not_found -> (try StringMap.find str env.env_fformals with
              Not_found -> StringMap.find str env.env_globals) in
      if lval <> typ then raise(Failure("variable " ^ str ^ " is of type " ^ string_of_typ lval ^ " when " ^ string_of_typ typ ^ " was expected"));


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
                | Float, Float  -> SBinop(s1, op, s2, Bool)
                | Node(dt1), Node(dt2) when dt1 = dt2 -> SBinop(s1, op, s2, Bool)
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
        let (s, nenv) = convert_expr (List.hd e_lst) env in 
        let t = get_sexpr_type s in
        let strcall = 
            match t with 
                Int         -> "print"
                | String    -> "prints"
                | Bool      -> "printb"
                | _         -> 
                    raise(Failure("type " ^ string_of_typ t ^ " is unsupported for this function"))
        in
        SCall(strcall, [s], Void), nenv


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
        if lvaluet = rvaluet then SAssign(str, r, lvaluet), env
        else
          (* The parser always says an edgeless graph literal is of type Graph,
             but it is a valid rvalue for Digraph, Wegraph, and Wedigraph too -
             if this is why lvaluet <> rvaluet, this is fine.
             Similarly, semant always says the graph literal  is of type Graph<Void>, 
             but it is always a valid rvalue for any type of Graph with any data type. *)
          (match (lvaluet, r) with
             (Digraph(t1), SGraph_Lit (_, [], _, Graph(t2), _)) when t1 = t2 -> SAssign(str, r, lvaluet), env
           | (Wegraph(t1), SGraph_Lit (_, [], _, Graph(t2), _)) when t1 = t2 -> SAssign(str, r, lvaluet), env
           | (Wedigraph(t1), SGraph_Lit (_, [], _, Graph(t2), _)) when t1 = t2 -> SAssign(str, r, lvaluet), env
           | (Graph(_), SGraph_Lit (_, [], _, Graph(Void), _))
           | (Digraph(_), SGraph_Lit (_, [], _, Graph(Void), _))
           | (Wegraph(_), SGraph_Lit (_, [], _, Graph(Void), _))
           | (Wedigraph(_), SGraph_Lit (_, [], _, Graph(Void), _)) -> SAssign(str, r, lvaluet), env
           | _ -> report_bad_assign lvaluet rvaluet
          )
    else report_undeclared_id_assign str


and check_data e e_lst env =
    let len = List.length e_lst in
    if (len != 0) then (raise(Failure("data takes 0 arguments but " ^ string_of_int len ^ " arguments given")))
    else
      let (s,env) = convert_expr e env in
      let t = get_sexpr_type s in
      match t with
        Node(dt) -> SMethod(s, "data", [], dt), env
      | _ -> raise(Failure("data() called on type " ^ string_of_typ t ^ " when node was expected"))

and check_sdata e e_lst env =
    let len = List.length e_lst in
    if (len != 1) then raise(Failure("set_data() takes 1 arguments but " ^ string_of_int len ^ " arguments given"))
    else
      let (id,env) = convert_expr e env in
      (* TODO: GET NEW ENVIRONMENT HERE! like in check_mapmtd *)
      let id_t = get_sexpr_type id in
      let (arg, env) = convert_expr (List.hd e_lst) env in
      let arg_t = get_sexpr_type arg in
      match (id_t, arg_t) with 
        (Node(dt), at) when dt = at -> SMethod(id, "set_data", [arg], Void), env
      | (Node(dt), at) -> raise(Failure("set_data() called on type " ^ string_of_typ id_t ^ " with parameter " ^
                                        string_of_typ at ^ " when " ^ string_of_typ dt ^ " was expected"))
      | (_, _) -> raise(Failure("set_data() called on type " ^ string_of_typ id_t ^ " when node was expected"))

and check_graphmtd g name args e_lst ret_typ env =
    let id,env = convert_expr g env in
    let t = get_sexpr_type id in
    let data_type = match t with Graph(dt) -> dt
                               | Digraph(dt) -> dt
                               | Wegraph(dt) -> dt
                               | Wedigraph(dt) -> dt
                               | _ -> 
                              raise(Failure(name ^ " called on type " ^ string_of_typ t ^ " when graph was expected")) in
    let len = List.length e_lst in
    if (len != args) then raise(Failure( name ^ " takes " ^ string_of_int args ^ " arguments but " ^ string_of_int len ^ " arguments given"));

    let sexpr,env =  
      match args with
      0 (* print *) -> (match data_type with
          | Int | Float | Bool | String -> (SMethod(id, name, [], ret_typ)), env
          | _ -> raise(Failure("print cannot be called on graphs with generic data types")))
    | 1 -> (
          let e1 = (List.hd e_lst) in
          let (ex,nenv) = convert_expr e1 env in
          let t1 = get_sexpr_type ex in
          if ( t1 <> Node(data_type) )
          then raise(Failure("graph method " ^ name ^ " may not be called on graph of type " ^ string_of_typ t ^
                             " with parameter " ^ string_of_typ t1));
                let new_env = {
                  env_name = env.env_name;
                  env_return_type = env.env_return_type;
                  env_fmap = env.env_fmap;
                  env_sfmap = nenv.env_sfmap;
                  env_globals = env.env_globals;
                  env_flocals = env.env_flocals;
                  env_fformals = env.env_fformals;
                  env_in_loop = env.env_in_loop;
                } in
                let ret_typ = if (ret_typ = Graph(Int)) then Graph(data_type) (* neighbors *)
                  else ret_typ in
          (SMethod(id, name, [ex], ret_typ)), new_env)

    | 2 -> 
      let e1 = (List.hd e_lst) and e2 = (List.nth e_lst 1) in
      let (ex,env1) = convert_expr e1 env in
      let (ex2,env2) = convert_expr e2 env1 in
      let t1 = get_sexpr_type ex and t2 = get_sexpr_type ex2 in 
      if ( t1 <> Node(data_type) || t2 <> Node(data_type) )
      then raise(Failure("graph method " ^ name ^ " may not be called on graph of type " ^ string_of_typ t ^
                         " with parameters " ^ string_of_typ t1 ^ ", " ^ string_of_typ t2));
      (* make sure this method can be called on this type *)
      if (name = "get_edge_weight" && (t = Graph(data_type) || t = Digraph(data_type)))
      then raise(Failure(name ^ " may not be called on unweighted graphs"));
      if (name = "add_edge" && (t = Wegraph(data_type) || t = Wedigraph(data_type)))
      then raise(Failure(name ^ " may not be called on weighted graphs without a weight argument"));
              let new_env = {
                env_name = env.env_name;
                env_return_type = env.env_return_type;
                env_fmap = env.env_fmap;
                env_sfmap = env2.env_sfmap;
                env_globals = env.env_globals;
                env_flocals = env.env_flocals;
                env_fformals = env.env_fformals;
                env_in_loop = env.env_in_loop;
            } in
      (SMethod(id, name, [ex; ex2], ret_typ)), new_env
    | 3 ->
      let e1 = (List.hd e_lst)
      and e2 = (List.nth e_lst 1)
      and e3 = (List.nth e_lst 2) in
      let (ex,env1) = convert_expr e1 env in
      let (ex2,env2) = convert_expr e2 env1 in
      let (ex3,env3) = convert_expr e3 env2 in
      let t1 = get_sexpr_type ex
      and t2 = get_sexpr_type ex2
      and t3 = get_sexpr_type ex3 in
      if ( t1 <> Node(data_type) || t2 <> Node(data_type) || t3 <> Int)
      then raise(Failure("graph method " ^ name ^ " may not be called on graph of type " ^ string_of_typ t ^
                         " with parameters " ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ", " ^ string_of_typ t3));
      (* all 3-argument graph methods can only be called on we(di)graphs *)
      if (t = Graph(data_type) || t = Digraph(data_type)) then
        (if (name = "add_edge") then
          raise(Failure(name ^ " may not be called on unweighted graphs with a weight argument"));
         raise(Failure(name ^ " may not be called on unweighted graphs")););
              let new_env = {
                env_name = env.env_name;
                env_return_type = env.env_return_type;
                env_fmap = env.env_fmap;
                env_sfmap = env3.env_sfmap;
                env_globals = env.env_globals;
                env_flocals = env.env_flocals;
                env_fformals = env.env_fformals;
                env_in_loop = env.env_in_loop;
            } in
      (SMethod(id, name, [ex; ex2; ex3], ret_typ)), new_env
  in sexpr, env


and check_edgemtd e n e_lst env = 
    let len = List.length e_lst in

    let rec add_sexpr acc e_lst env = match e_lst with
      [] -> acc, env
      | e :: e_lst -> 
            let se, new_env = convert_expr e env in 
            let new_acc = se::acc in add_sexpr new_acc e_lst new_env
    in
    let e_lst_checked, nenv = add_sexpr [] e_lst env in
    let new_env = {
          env_name = env.env_name;
          env_return_type = env.env_return_type;
          env_fmap = env.env_fmap;
          env_sfmap = nenv.env_sfmap;
          env_globals = env.env_globals;
          env_flocals = env.env_flocals;
          env_fformals = env.env_fformals;
          env_in_loop = env.env_in_loop;
        }
    in

    let correct_len = match n with "set_weight" -> 1 | _ -> 0 in
    if (len != correct_len) then
      raise(Failure(n ^ " takes " ^ string_of_int correct_len ^ " arguments but " ^ string_of_int len ^ " arguments given")) else
      let se, nenv = convert_expr e new_env in
      let t = get_sexpr_type se in
      let new_env = {
              env_name = env.env_name;
              env_return_type = env.env_return_type;
              env_fmap = env.env_fmap;
              env_sfmap = nenv.env_sfmap;
              env_globals = env.env_globals;
              env_flocals = env.env_flocals;
              env_fformals = env.env_fformals;
              env_in_loop = env.env_in_loop;
      }
      in
      match t with
        Diwedge(dt) | Wedge(dt) ->
        (match n with
           "from" -> SMethod(se, n, List.rev e_lst_checked, Node(dt)), new_env
         | "to" -> SMethod(se, n, List.rev e_lst_checked, Node(dt)), new_env
         | "weight" -> SMethod(se, n, List.rev e_lst_checked, Int), new_env
         | "set_weight" -> SMethod(se, n, List.rev e_lst_checked, Void), new_env)
      | Edge(dt) -> 
        (match n with
           "from" -> SMethod(se, n, List.rev e_lst_checked, Node(dt)), new_env
         | "to" -> SMethod(se, n, List.rev e_lst_checked, Node(dt)), new_env
         | "weight" -> raise(Failure("weight() cannot be called on edges of unweighted graphs"));
         | "set_weight" -> raise(Failure("set_weight() cannot be called on edges of unweighted graphs"));)
      | _ -> raise(Failure("Edge method " ^ n ^ " called on type " ^ string_of_typ t));

and check_mapmtd m name args e_lst env =
  let id,env = convert_expr m env in
  let t = get_sexpr_type id in
  let value_typ = match t with
    Map(v) -> v
  | _ -> raise(Failure(name ^ " called on type " ^ string_of_typ t ^ " when map was expected")) in
  let len = List.length e_lst in
  if (len != args) then raise(Failure( name ^ " takes " ^ string_of_int args ^ " arguments but " ^ string_of_int len ^ " arguments given")) ;

  let sexpr,env =
    match args with
      1 -> ( (* get(k), contains(k) *)
        let e1 = (List.hd e_lst) in
        let (ex,nenv) = convert_expr e1 env in
        let t1 = get_sexpr_type ex in
        (match t1 with
          Node(dt) -> ()
        | _ -> raise(Failure("map method " ^ name ^ " may not be called on type " ^ string_of_typ t1)));
        let new_env = {
          env_name = env.env_name;
          env_return_type = env.env_return_type;
          env_fmap = env.env_fmap;
          env_sfmap = nenv.env_sfmap;
          env_globals = env.env_globals;
          env_flocals = env.env_flocals;
          env_fformals = env.env_fformals;
          env_in_loop = env.env_in_loop;
        } in
        let ret_typ = (match name with "contains" -> Bool | _ -> value_typ) in
        (SMethod(id, name, [ex], ret_typ)), new_env)

    | 2 -> (* put(k,v) *)
      let e1 = (List.hd e_lst) and e2 = (List.nth e_lst 1) in
      let (ex,env1) = convert_expr e1 env in
      let (ex2,env2) = convert_expr e2 env1 in
      let t1 = get_sexpr_type ex and t2 = get_sexpr_type ex2 in
      (match t1 with
          Node(dt) -> ()
        | _ -> raise(Failure("map method " ^ name ^ " may not be called with key type "
                         ^ string_of_typ t1)));
      if ( t2 <> value_typ )
      then raise(Failure("map method " ^ name ^ " called with value type "
                         ^ string_of_typ t2 ^ " on map of type " ^ string_of_typ value_typ ));
      let new_env = {
        env_name = env.env_name;
        env_return_type = env.env_return_type;
        env_fmap = env.env_fmap;
        env_sfmap = env2.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = env.env_flocals;
        env_fformals = env.env_fformals;
        env_in_loop = env.env_in_loop;
      } in
      (SMethod(id, name, [ex; ex2], Void)), new_env
  in sexpr, env

(* TODO *)
and check_graph str_lst ed_lst n_lst is_d is_w env =
    let graph_type data_type = match (is_d, is_w) with
        (true, true) -> Wedigraph(data_type)
      | (true, false) -> Digraph(data_type)
      | (false, true) -> Wegraph(data_type)
      | (false, false) -> Graph(data_type)
    in
    (* convert each weight expression *)
    let ed_lst_checked = List.map (fun (f,t,w) -> (f, t, fst (convert_expr w env))) ed_lst in
    let weight_types = List.map (fun (_,_,w_sexpr) -> get_sexpr_type w_sexpr) ed_lst_checked in
    List.iter (fun t -> if t <> Int then
                  raise (Failure("edge weights must be of type int"))) weight_types;
    (* make sure no nodes are initialized more than once *)
    ignore(List.fold_left (fun m (n, _)  -> if StringMap.mem n m then
                              raise(Failure("graph literal cannot initialize the same node more than once"))
                            else StringMap.add n true m) StringMap.empty n_lst);
    (* make sure no edge appears more than once (may happen with we(di)graphs) *)
    ignore(List.fold_left (fun m (f,t,_)  -> if StringMap.mem (f ^ "+" ^ t) m then
                              raise(Failure("graph literal cannot feature the same edge with different weights"))
                            else StringMap.add (f ^ "+" ^ t) true m) StringMap.empty ed_lst);
    match str_lst with
      [] -> SGraph_Lit([], [], [], Graph(Void), Void), env
    | _ ->
      let is_declared env str = (StringMap.mem str env.env_flocals ||
                                 StringMap.mem str env.env_fformals ||
                                 StringMap.mem str env.env_globals) in
      let get_declared_type env str =
        let node_type = 
          if (StringMap.mem str env.env_flocals) then
            StringMap.find str env.env_flocals
          else if (StringMap.mem str env.env_fformals) then
            StringMap.find str env.env_fformals
          else
            StringMap.find str env.env_globals
        in match node_type with Node(t) -> t | _ -> node_type
      in
      let declared = List.filter (is_declared env) str_lst in
      let declared_types = List.map (get_declared_type env) declared in
      let rec check_consistent types = (match types with
            [] -> true
          | [_] -> true
          | hd :: tl -> (hd = List.hd tl) && (check_consistent tl))
      in
      if (not (check_consistent declared_types)) then
        raise(Failure("all nodes in graph literal must have the same data type"));
      (match (declared_types, n_lst) with
         [], [] -> raise(Failure("graph literal must contain at least one previously " ^
                                 "declared node or at least one initialized node"));
       | _, [] -> let t = List.hd declared_types in
         let newenv = List.fold_left (fun x y -> let (_, z) = check_vdecl (Node(t)) y Noexpr true x in z) env str_lst in
         SGraph_Lit(str_lst, ed_lst_checked, [], (graph_type t), t), newenv
       | [], _ -> let (s,_) = convert_expr (snd (List.hd n_lst)) env in
         let t = get_sexpr_type s in
         List.iter (fun n -> let (sn,_) = convert_expr (snd n) env in
                     let tn = get_sexpr_type sn in
                     if tn <> t then raise (Failure("node type mismatch of " ^ string_of_typ t ^ " and " ^ string_of_typ tn))) n_lst;
         let newenv = List.fold_left (fun x y -> let (_, z) = check_vdecl (Node(t)) y Noexpr true x in z) env str_lst in
         let nodes = List.map (fun (x,y) -> let (s,_) = convert_expr y newenv in (x,s)) n_lst
         in
         (SGraph_Lit(str_lst, ed_lst_checked, nodes, (graph_type t), t), newenv)
       | _, _ ->
         let n_lst_types = List.map (fun n -> (get_sexpr_type (fst (convert_expr (snd n) env)))) n_lst in
         if (check_consistent (declared_types @ n_lst_types)) then
           let t = List.hd declared_types in
           let newenv = List.fold_left (fun x y -> let (_, z) = check_vdecl (Node(t)) y Noexpr true x in z) env str_lst in
           let nodes = List.map (fun (x,y) -> let (s,_) = convert_expr y newenv in (x,s)) n_lst
           in (SGraph_Lit(str_lst, ed_lst_checked, nodes, (graph_type t), t), newenv)
         else
           raise(Failure("all nodes in graph literal must have the same data type"));)

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
        let fdecl = StringMap.find str env.env_fmap
        in
        let nenv = 
        {
            env_name = env.env_name;
            env_return_type = env.env_return_type;
            env_fmap = env.env_fmap;
            env_sfmap = (convert_fdecl fdecl.f_name fdecl.f_formals env).env_sfmap;
            env_globals = env.env_globals;
            env_flocals = env.env_flocals;
            env_fformals = env.env_fformals;
            env_in_loop = env.env_in_loop;
        }
        in

        let sfdecl = StringMap.find str nenv.env_sfmap
        in try
              (* confirm types match *)
              List.iter2 (fun t1 (t2, _) -> if t1 <> t2 then report_typ_args t2 t1 else ()) arg_types sfdecl.sf_formals;
              let sexpr_lst, env = convert_expr_list e_lst env in 
                  SCall(str, sexpr_lst, sfdecl.sf_typ), nenv
          with 
          (* wrong number of arguments *)
              Invalid_argument _ -> raise (Failure ("expected " ^ string_of_int (List.length sfdecl.sf_formals) ^ "arguments when " 
              ^ string_of_int (List.length e_lst) ^ " arguments were provided"))
    else
          let sfdecl = StringMap.find str env.env_sfmap in
          try 
              (* confirm types match *)
              List.iter2 (fun t1 (t2, _) -> if t1 <> t2 then report_typ_args t2 t1 else ()) arg_types sfdecl.sf_formals;
              let sexpr_lst, env = convert_expr_list e_lst env in 
                  SCall(str, sexpr_lst, sfdecl.sf_typ), env
          with 
          (* wrong number of arguments *)
              Invalid_argument _ -> raise (Failure ("expected " ^ string_of_int (List.length sfdecl.sf_formals) ^ "arguments when " 
              ^ string_of_int (List.length e_lst) ^ " arguments were provided"))

and convert_stmt stmt env = match stmt with 
  Block(s_lst)            -> (check_block s_lst env)
  | If(e, s1, s2)         -> (check_if e s1 s2 env)
  | For(e1, e2, e3, s)    -> (check_for e1 e2 e3 s env) 
  | While(e, s)           -> (check_while e s env)
  | For_Node(str, e, s)   -> (check_for_node str e s env)
  | For_Edge(str, e, s)   -> (check_for_edge str e s env)
  | Bfs(str, e1, e2, s)   -> (check_bfs str e1 e2 s env)
  | Dfs(str, e1, e2, s)   -> (check_dfs str e1 e2 s env) 
  | Break                 -> (check_break env, env)  
  | Continue              -> (check_continue env, env) 
  | Expr(e)               -> (check_expr_stmt e env) 
  | Vdecl(t, str, e)      -> (check_vdecl t str e false env)
  | Return(e)             -> (check_return e env)


and check_block s_lst env = 
    match s_lst with 
    []    -> SBlock([SExpr(SNoexpr, Void)]), env
    | _   -> (*check every statement, and put those checked statements in a list*)
              let rec add_sstmt acc stmt_lst env = match stmt_lst with
                [] -> acc, env
                | Return _ :: _ :: _  -> raise (Failure("nothing may follow a return"))
                | st :: st_lst -> 
                      let sstmt, new_env = convert_stmt st env in 
                      let new_acc = sstmt::acc in add_sstmt new_acc st_lst new_env
              in
              let sblock, nenv = add_sstmt [] s_lst env in
              let new_env = {
                env_name = env.env_name;
                env_return_type = env.env_return_type;
                env_fmap = env.env_fmap;
                env_sfmap = nenv.env_sfmap;
                env_globals = env.env_globals;
                env_flocals = env.env_flocals;
                env_fformals = env.env_fformals;
                env_in_loop = env.env_in_loop;
            }
            in 
            (SBlock(List.rev sblock), new_env)

and check_if cond is es env =
    (* semantically check the condition *)
    let scond,env = convert_expr cond env in 
        (match get_sexpr_type scond with 
            Bool ->
                let (sis, env) = convert_stmt is env in
                let (ses, env) = convert_stmt es env in
                SIf(scond, sis, ses), env
            | _ -> raise(Failure("Expected boolean expression")))    


and check_for e1 e2 e3 s env = 
    let (se1, env1) = convert_expr e1 env in (* a new var may be added to locals there*)
    let (se2, env2) = convert_expr e2 env1 in
    let (se3, env3) = convert_expr e3 env2 in
    let new_env = 

    {
      env_name = env3.env_name;
      env_return_type = env3.env_return_type;
      env_fmap = env3.env_fmap;
      env_sfmap = env3.env_sfmap;
      env_globals = env3.env_globals;
      env_flocals = env3.env_flocals;
      env_fformals = env3.env_fformals;
      env_in_loop = true;
    }
    in

    let (for_body, nenv) = convert_stmt s new_env in

    (* check if you have a return in a for *)
    match for_body with
      SBlock(slst) ->
          let rets = List.filter (fun x -> match x with 
                                      SReturn(expr)-> true
                                      | _ -> false ) slst 
          in
          if List.length rets != 0 then raise(Failure("cannot return in for loop"));


    let nnenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = nenv.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    if (get_sexpr_type se2) = Bool then
        SFor(se1, se2, se3, for_body), nnenv
    else raise(Failure("Expected boolean expression"))


and check_while e s env =
    let (se, nenv) = convert_expr e env in
    let new_env = 
    {
      env_name = nenv.env_name;
      env_return_type = nenv.env_return_type;
      env_fmap = nenv.env_fmap;
      env_sfmap = nenv.env_sfmap;
      env_globals = nenv.env_globals;
      env_flocals = nenv.env_flocals;
      env_fformals = nenv.env_fformals;
      env_in_loop = true;
    }
    in
    let (while_body, nenv) = convert_stmt s new_env in
    
    let nnenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = nenv.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    if (get_sexpr_type se) = Bool then
        SWhile(se, while_body), nnenv
    else raise(Failure("Expected boolean expression"))

and check_for_node str e s env =
    (* This is jank as hell but it's the last day so whatever *)
    let graph_type = get_sexpr_type (fst (convert_expr e env)) in
    let node_type = match graph_type with
        Wedigraph(dt) | Wegraph(dt) | Graph(dt) | Digraph(dt) -> Node(dt)
      | _ -> raise(Failure("type " ^ string_of_typ graph_type ^
                           " may not be iterated with for_node")); (* TODO: write a test for this *)
    in
    let flocals = StringMap.add str node_type env.env_flocals in
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
    let gname = match se with (* cannot call the following methods on a NAMED graph*)
         (* unnamed graph is safe since it cannot modify the graph itself *)    
          SId(str, Graph(_)) -> str
        | SId(str, Digraph(_)) -> str
        | SId(str, Wedigraph(_)) -> str
        | SId(str, Wegraph(_)) -> str
        | _ -> "" (*not sure if empty string is problematic *)
    in 
    let (for_body, senv) = convert_stmt s senv in
    let chk = match for_body with
       SBlock(lst) -> match lst with
                [] -> ();
                | _ -> (* non empty statement lst, check em*)
                let chkcall x = match x with 
                SExpr(SMethod(g,fname,_,_),_) -> (match g,fname with
                        SId(s,_),"add_node" -> if s = gname then report_concurrent_mod "for_node"
                        | SId(s,_),"remove_node" -> if s = gname then report_concurrent_mod "for_node"
                        | _ -> ();)
                | _ -> ();
                in
                List.iter chkcall lst
       | _ -> ()
    in
    let nenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = senv.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    SFor_Node(str, se, for_body),nenv

and check_for_edge str e s env =
    let graph_type = get_sexpr_type (fst (convert_expr e env)) in
    let edge_type = match graph_type with
        Wedigraph(dt) -> Diwedge(dt)
      | Wegraph(dt) -> Wedge(dt)
      | Graph(dt) | Digraph(dt) -> Edge(dt)
      | _ -> raise(Failure("type " ^ string_of_typ graph_type ^
                           " may not be iterated with for_edge")); (* TODO: write a test for this *)
    in
    let flocals = StringMap.add str edge_type env.env_flocals in
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
      let gname = match se with (* cannot call the following methods on a NAMED graph*)
           (* unnamed graph is safe since it cannot modify the graph itself *)    
            SId(str, Graph(_)) -> str
          | SId(str, Digraph(_)) -> str
          | SId(str, Wedigraph(_)) -> str
          | SId(str, Wegraph(_)) -> str
          | _ -> "" (* not sure if empty string is problematic *)
      in 
      let (for_body, senv) = convert_stmt s senv in
      let chk = match for_body with
         SBlock(lst) -> match lst with
                  [] -> ();
                  | _ -> (* non empty statement lst, check em *)
                  let chkcall x = match x with 
                  SExpr(SMethod(g,fname,_,_),_) -> (match g,fname with
                          SId(s,_),"add_edge" -> if s = gname then report_concurrent_mod "for_edge"
                          | SId(s,_),"remove_edge" -> if s = gname then report_concurrent_mod "for_edge"
                          | SId(s,_),"add_node" -> if s = gname then report_concurrent_mod "for_edge"
                          | SId(s,_),"remove_node" -> if s = gname then report_concurrent_mod "for_edge"
                          | _ -> ();)
                  | _ -> ();
                  in
                  List.iter chkcall lst
         | _ -> ()
      in
      let nenv = 
      {
        env_name = env.env_name;
        env_return_type = env.env_return_type;
        env_fmap = env.env_fmap;
        env_sfmap = senv.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = env.env_flocals;
        env_fformals = env.env_fformals;
        env_in_loop = env.env_in_loop;
      }
      in
      SFor_Edge(str, se, for_body),nenv


and check_bfs str e1 e2 s env =
    let graph_type = get_sexpr_type (fst (convert_expr e1 env)) in
    let node_type = match graph_type with
        Wedigraph(dt) | Wegraph(dt) | Graph(dt) | Digraph(dt) -> Node(dt)
      | _ -> raise(Failure("type " ^ string_of_typ graph_type ^
                           " may not be iterated with bfs"));
    in
    let flocals = StringMap.add str node_type env.env_flocals in
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
    let source_type = get_sexpr_type se2 in
    (match source_type with
      Node(_) -> ()
    | _ -> raise(Failure("source expr in bfs must be of type node")));
    let gname = match se1 with (* cannot call the following methods on a NAMED graph*)
          SId(str, Graph(_)) -> str
        | SId(str, Digraph(_)) -> str
        | SId(str, Wedigraph(_)) -> str
        | SId(str, Wegraph(_)) -> str
        | _ -> "" (*not sure if empty string is problematic *)
    in 
    let (bfs_body, senv2) = convert_stmt s senv2 in 
    let chk = match bfs_body with
       SBlock(lst) -> match lst with
                [] -> ();
                | _ -> (* non empty statement lst, check em*)
                let chkcall x = match x with 
                SExpr(SMethod(g,fname,_,_),_) -> (match g,fname with
                        SId(s,_),"add_edge" -> if s = gname then report_concurrent_mod "bfs"
                        | SId(s,_),"remove_edge" -> if s = gname then report_concurrent_mod "bfs"
                        | SId(s,_),"add_node" -> if s = gname then report_concurrent_mod "bfs"
                        | SId(s,_),"remove_node" -> if s = gname then report_concurrent_mod "bfs"
                        | _ -> ();)
                | _ -> ();
                in
                List.iter chkcall lst
       | _ -> ()
    in
    let nenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = senv2.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    SBfs(str, se1, se2, bfs_body), nenv


and check_dfs str e1 e2 s env = 
    let graph_type = get_sexpr_type (fst (convert_expr e1 env)) in
    let node_type = match graph_type with
        Wedigraph(dt) | Wegraph(dt) | Graph(dt) | Digraph(dt) -> Node(dt)
      | _ -> raise(Failure("type " ^ string_of_typ graph_type ^
                           " may not be iterated with dfs"));
    in
    let flocals = StringMap.add str node_type env.env_flocals in
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
    let source_type = get_sexpr_type se2 in
    (match source_type with
      Node(_) -> ()
    | _ -> raise(Failure("source expr in dfs must be of type node")));
    let gname = match se1 with (* cannot call the following methods on a NAMED graph*)
          SId(str, Graph(_)) -> str
        | SId(str, Digraph(_)) -> str
        | SId(str, Wedigraph(_)) -> str
        | SId(str, Wegraph(_)) -> str
        | _ -> "" (*not sure if empty string is problematic *)
    in 
    let (dfs_body, senv2) = convert_stmt s senv2 in 
    let chk = match dfs_body with
       SBlock(lst) -> match lst with
                [] -> ();
                | _ -> (* non empty statement lst, check em*)
                let chkcall x = match x with 
                SExpr(SMethod(g,fname,_,_),_) -> (match g,fname with
                        SId(s,_),"add_edge" -> if s = gname then report_concurrent_mod "dfs"
                        | SId(s,_),"remove_edge" -> if s = gname then report_concurrent_mod "dfs"
                        | SId(s,_),"add_node" -> if s = gname then report_concurrent_mod "dfs"
                        | SId(s,_),"remove_node" -> if s = gname then report_concurrent_mod "dfs"
                        | _ -> ();)
                | _ -> ();
                in
                List.iter chkcall lst
    in
    let nenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = senv2.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    SDfs(str, se1, se2, dfs_body), nenv


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
          (t, str) -> match t with 
            Void -> raise(Failure("cannot declare " ^ str ^ " as type void")) 
          | Map(Void) | Graph(Void) | Digraph(Void) | Wegraph(Void) | Wedigraph(Void) ->
            raise(Failure("cannot have formal with type " ^ string_of_typ t))
          | _ -> StringMap.add str t m
    in

    let formals = List.fold_left formals_to_map StringMap.empty fformals 
    in 

    let _ = match fdecl.f_typ with 
        Map(Void) | Graph(Void)  | Digraph(Void) | Wegraph(Void) | Wedigraph(Void) ->
        raise(Failure("cannot return " ^ string_of_typ fdecl.f_typ))
      | _ -> ()
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
    let (sstmts, nenv ) = convert_stmt (Block fdecl.f_body) env
    in 
    let check_ret = match sstmts with 
        SBlock(lst) -> match lst with
        [] -> raise(Failure("missing return in " ^ fdecl.f_name));
        | _ -> 
                let rets = List.filter (fun x -> match x with 
                        SReturn(expr)-> true
                        | _ -> false ) lst in
                if List.length rets > 1 then 
                raise(Failure("misplaced return in " ^ fdecl.f_name))
                else
                match (List.nth lst ((List.length lst)-1)) with
                SReturn(sexpr) -> ()
                | _ -> raise(Failure("missing return in " ^ fdecl.f_name))
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
        env_sfmap = StringMap.add fname sfdecl nenv.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = env.env_flocals;
        env_fformals = formals;
        env_in_loop = env.env_in_loop
    } 
  in new_env
    

and check_vdecl t str e from_graph_lit env =
    let t_is_node = match t with Node(_) -> true | _ -> false in
    if (StringMap.mem str env.env_flocals || StringMap.mem str env.env_fformals || StringMap.mem str env.env_globals)
    then
      (* if this vdecl is from a graph literal and we've already declared str 
         as this type of node, this is fine - otherwise, reject *)
      if (from_graph_lit && t_is_node) then
         (if (StringMap.mem str env.env_flocals) then
            (if (StringMap.find str env.env_flocals <> t) then
               raise(Failure("cannot reinitialize existing variable")))
          else if (StringMap.mem str env.env_fformals) then
            (if (StringMap.find str env.env_fformals <> t) then
               raise(Failure("cannot reinitialize existing variable")))
          else if (StringMap.mem str env.env_globals) then
            (if (StringMap.find str env.env_globals <> t) then
               raise(Failure("cannot reinitialize existing variable")))
          else raise(Failure("cannot reinitialize existing variable")))
        else raise(Failure("cannot reinitialize existing variable"));

    let _ = match t with 
        Void -> raise(Failure("cannot declare " ^ str ^ " as type void"))
      | Map(Void) | Graph(Void) | Digraph(Void) | Wegraph(Void) | Wedigraph(Void) ->
        raise(Failure("cannot declare variable " ^ str ^ " with type " ^ string_of_typ t))
      | _ -> ()
    in

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
    if typ <> Void then
      (if t <> typ then (raise(Failure("expression type mismatch " ^ string_of_typ t ^ " and " ^ string_of_typ typ)))
      else (SVdecl(t, str, se), nenv))
    else 
       (SVdecl(t, str, se), nenv)


and check_return e env =
    let se, new_env = convert_expr e env in 
    let typ = get_sexpr_type se in
    if typ = env.env_return_type 
    then
    let nenv = 
    {
      env_name = env.env_name;
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = new_env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in
    SReturn(se), nenv
    else raise(Failure("expected return type " ^ string_of_typ env.env_return_type 
      ^ " but got return type " ^ string_of_typ typ))

let convert_ast globals fdecls fmap = 
    let _ = try StringMap.find "main" fmap with
        Not_found -> raise(Failure("missing main")) 
    in

    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

    let convert_globals m global = 
      match global with
      (typ, str) -> if (typ <> Void) then StringMap.add str typ m
      else raise(Failure("global " ^ str ^ " cannot have a void type"))
    in

    (* semantically checked globals in a map *)
    let globals_map = List.fold_left convert_globals StringMap.empty globals
    in 

    (* check for duplicate functions *)
    report_duplicate (fun f -> "duplicate function " ^ f.f_name) fdecls;
   
    (*List.iter (fun x -> convert_fdecl x x.f_name env) fdecls; *)
    
 
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
    in (globals, sfdecls)


let build_fmap fdecls = 
    (* built in *)
    let built_in_decls =  StringMap.add "print"
     { f_typ = Void; f_name = "print"; f_formals = [(Int, "x")];
       f_body = [] } (StringMap.add "printb"
     { f_typ = Void; f_name = "printb"; f_formals = [(Bool, "x")];
       f_body = [] } (StringMap.singleton "prints"
     { f_typ = Void; f_name = "prints"; f_formals = [(String, "x")];
       f_body = [] } ))
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
