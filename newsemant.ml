(* Semantic checking for the giraph compiler *)

open Ast
open Sast
open Exception (*need to make better lmao*)

module StringMap = Map.Make(String)

type env = {
  env_name : string; (* name of fn*)
  env_return_type : typ; (* return type *)
  env_fmap : fdecl StringMap.t; (* function map *)
  env_sfmap : sfdecl StringMap.t; (* do we need this? *)
  env_globals : typ StringMap.t; (* global vars *)
  env_flocals : typ StringMap.t; (* function locals *)
  env_fformals : typ StringMap.t; (* function formals *)
  env_in_while : bool;
  (* todo: built in methods? *)
}

let rec convert_expr e env = match e with 
    Id(str)                       -> (check_id str env, env)
  | Binop(e1, op, e2)             -> (check_binop e1 op e2 env, env) 
  | Unop(op, e)                   -> (check_unop op e env, env)
  | Assign(str, e)                -> (check_assign str e env, env)
  (*todo below*)
  | Call("print", e_lst)          -> (check_print e_lst env, env)
  | Call("printb", e_lst)         -> (check_print e_lst env, env)
  | Call("prints", e_lst)         -> (check_print e_lst env, env)
  | Call(str, e_lst)              -> (check_call str e_lst env, env)
  | Bool_Lit(b)                   -> (SBool_Lit(b), env)
  | Int_Lit(i)                    -> (SInt_Lit(i), env)
  | Float_Lit(f)                  -> (SFloat_Lit(f), env)
  | String_Lit(str)               -> (SString_Lit(str), env)
  (* todo below *)
  | Node(n)                       -> (SNode(n, Node), env)
  | Edge(ed)                      -> (SEdge(ed, Edge), env)
  | Graph(str_lst, ed_lst)        -> (SGraph(str_lst, ed_lst, Graph), env)
  | Noexpr                        -> (SNoexpr, env)


and convert_expr_list expr_lst env = 
    let sexpr_env_lst = List.map (fun expr -> convert_expr expr env) expr_lst in
    let sexpr_lst = List.map (fun (sexpr, _) -> sexpr) sexpr_env_lst in
    sexpr_lst, env


and get_sexpr_type sexpr = match sexpr with
    SId(_, typ)                -> typ
  | SBinop(_, _, _, typ)       -> typ
  | SUnop(_, _, typ)           -> typ
  | SAssign(_, _, typ)         -> typ
  | SCall(_, _, typ)           -> typ
  | SBool_Lit(_)               -> Bool
  | SInt_Lit(_)                -> Int
  | SFloat_Lit(_)              -> Float
  | SString_Lit(_)             -> String
  | SNode(_, typ)              -> typ
  | SEdge(_, typ)              -> typ
  | SGraph(_, _, typ)          -> typ
  | SNoexpr                    -> Void


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
                report_undeclared_id str))


and check_binop e1 op e2 env =
    let (s1, env1) = convert_expr e1 env 
    and (s2, env2) = convert_expr e2 env in
    let t1 = get_sexpr_type s1 and t2 = get_sexpr_type s2 in
    (* TODO add checking if types are graph!! *)
    match op with
        Add ->
            (match t1, t2 with
                Int, Int        -> SBinop(s1, op, s2, Int)
                | Float, Float  -> SBinop(s1, op, s2, Float)
                | _             -> report_bad_binop t1 op t2
                (*TODO: string concat? *)
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
        | _ -> report_bad_binop t1 op t2


and check_unop op e env = 
    let (s, env) = convert_expr e env in
    let t = get_sexpr_type s in
    match op with 
        Neg when (t = Int || t = Float) -> SUnop(op, s, t)
        | Not when t = Bool             -> SUnop(op, s, t)
        | _ -> report_bad_unop op t


and check_print e_lst env = 
    if ((List.length e_lst) != 1) then raise(Failure("wrong number of arguments"))
    else
        let (s, env) = convert_expr (List.hd e_lst) env in 
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
        if lvaluet == rvaluet then SAssign(str, r, lvaluet) 
        else report_bad_assign lvaluet rvaluet
    else report_undeclared_id str

and check_call str e_lst env = 
    (* can't call main *)
    if str == "main" then raise (Failure ("cant make call to main"))
    (* check if function can be found*)
    else if not (StringMap.mem str env.env_sfmap) then report_function_not_found str
    else 
    (* semantically check all the arguments*)
    let checked_args, env = convert_expr_list e_lst env in 

    (* get the types of the args*)
    let arg_types = get_sexpr_lst_type (checked_args) in

    (* validate types here? *)

    let sfdecl = StringMap.find str env.env_sfmap in
    
    try 
        (* confirm types match *)
        List.iter2 (fun t1 (t2, _) -> if t1 != t2 then report_typ_args t2 t1 else ()) arg_types sfdecl.sf_formals;
        let sexpr_lst, env = convert_expr_list e_lst env in 
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
    | Break                 -> (check_break, env) 
    | Continue              -> (check_continue, env)
    | Expr(e)               -> (check_expr_stmt e env, env) 
    | Vdecl(t, str, e)      -> (check_vdecl t str e env)
    | Return(e)             -> (check_return e, env) (* REMEMBER TO SET RETURN TYPE when you're semantically checking everything
                                    in the fdecl block *)


and check_block s_lst env = function
    (*let rec check_block_helper = function
      Return _ :: _ -> raise (Failure("nothing may follow a return"))
      | Block s_lst :: ss -> check_block_helper (s_lst @ ss)
      | s :: ss -> ignore(convert_stmt s env); check_block_helper ss
      | [] -> ()
    in check_block_helper s_lst;*)
    []      -> SBlock([SExpr(SNoexpr, Void)])
    | _     -> SBlock(List.map (fun s -> check_block s env) s_lst)


and check_if cond is es env =
    (* semantically check the condition *)
    let scond,typ = convert_expr cond env in 
        (match typ with 
            Bool ->
                let (sis, _) = convert_stmt is env in
                let (ses, _) = convert_stmt es env in
                SIf(scond, sis, ses)
            | _ -> raise(Failure("Expected boolean expression")))    

and check_for e1 e2 e3 s env = 
    let (se1, nenv) = convert_expr e1 env in (* a new var may be added to locals there*)
    let (se2, env2) = convert_expr e2 nenv in
    let (se3, env3) = convert_expr e3 nenv in
    let new_env = 

    {
      env_return_type = nenv.env_return_type;
      env_fmap = nenv.env_fmap;
      env_sfmap = nenv.env_sfmap;
      env_globals = nenv.env_globals;
      env_flocals = nenv.env_flocals;
      env_fformals = nenv.env_fformals;
      env_in_loop = true;
    }
    in

    let (for_body, for_env) = convert_stmt s new_env in

    if (get_sexpr_type se2) = SBool then
        SFor(se1, se2, se3, for_body)
    else raise(Failure("Expected boolean expression"))

and check_while e s env =
    let (se, senv) = convert_expr e env in
    let new_env = 
    {
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_fformals = env.env_fformals;
      env_in_loop = true;
    }
    in

    let (while_body, while_env) = convert_stmt s new_env in
    if (get_sexpr_type se) = SBool then
        SWhile(se, while_body)
    else raise(Failure("Expected boolean expression"))

(* for_node (neighbor : residual.get_neighbors(n)) *)
and check_for_node str e s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
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
    let (for_body, for_env) = convert_stmt s new_env in
    SFor_Node(str, se, for_body)

and check_for_edge str e s env = 
    let flocals = StringMap.add str Edge env.env_flocals in
    let new_env = 
    {
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
    let (for_body, for_env) = convert_stmt s new_env in
    SFor_Edge(str, se, for_body)

and check_bfs str e1 e2 s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
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
    let (se2, senv2) = convert_expr e2 new_env in
    let (bfs_body, bfs_env) = convert_stmt s new_env in 
    SBfs(str, se1, se2, bfs_body)

and check_dfs str e1 e2 s env = 
    let flocals = StringMap.add str Node env.env_flocals in
    let new_env = 
    {
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
    let (se2, senv2) = convert_expr e2 new_env in
    let (dfs_body, dfs_env) = convert_stmt s new_env in 
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
    let (se, env) = convert_expr e env in 
    let typ = get_sexpr_type se in SExpr(se, typ)

and convert_fdecl fname fformals env = 
    let fdecl = StringMap.find fname env.env_fmap in
    
    let (sstmts, env) = convert_stmt (Block fdecl.body) env
    in 
    let formals = List.fold_left 
    in
    let sfdecl = { 
        sf_typ = env.env_return_type;
        sf_name = fdecl.f_name;
        sf_formals = formals;
        sf_body = match sstmts with SBlock(sl) -> sl | _ -> [] ;
    }

    in
    let env = {
        env_name = fname;
        env_return_type = env.env_return_type;
        env_fmap = env.env_fmap;
        env_sfmap = StringMap.add fname sfdecl env.env_sfmap;
        env_globals = env.env_globals;
        env_flocals = env.env_flocals;
        env_fformals = env.env_fformals;
    }
    in env    

and check_vdecl t str e env = 
    let (se, nenv) = convert_expr e env in
    let typ = get_sexpr_type se in
    if t != typ then raise(Failure("expression type mismatch"))
    else
    let flocals = StringMap.add str t env.env_flocals in
    let new_env = 
    {
      env_return_type = env.env_return_type;
      env_fmap = env.env_fmap;
      env_sfmap = env.env_sfmap;
      env_globals = env.env_globals;
      env_flocals = flocals;
      env_fformals = env.env_fformals;
      env_in_loop = env.env_in_loop;
    }
    in (SVdecl(t, str, se), new_env)

and check_return e env =
    let se, env = convert_expr e env in 
    let typ = get_sexpr_type se in
    if typ = env.env_return_type 
    then SReturn(se)
    else raise(Failure("expected return type " ^ string_of_typ env.env_return_type 
      ^ " but got return type " ^ string_of_typ typ))

let check_globals globals fmap = 
    List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
    report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals)


let convert_ast globals functions fmap = 
    let _ = try StringMap.find "main" fmap with
        Not_found -> raise(Failure("missing main")) 
    in

    (* lets get this started *)
    let env = {
        env_name = "main";
        env_return_type = Int;
        env_fmap = StringMap.empty;
        env_sfmap = StringMap.empty;
        env_globals = StringMap.empty;
        env_flocals = StringMap.empty;
        env_fformals = StringMap.empty;
    }
    in

    let sglobals = [] (*TODO check globals*)
    in 
    let globals_map = sglobals
    in

    let env = {
        env_name = "main";
        env_return_type = Int;
        env_fmap = fmap;
        env_sfmap = StringMap.empty;
        env_globals = globals_map;
        env_flocals = StringMap.empty;
        env_fformals = StringMap.empty;
    }
    in
    
    report_duplicate (List.map (fun f -> f.fname) functions);
    
    let env = convert_fdecls env in
    let env = convert_fdecls "main" [] env in 
    let sfdecls = List.rev(List.fold_left (fun l (_, sfdec) -> sfdec :: l)
                  [] (StringMap.bindings env.env_sfmap))
    in (sglobals, sfdecls)
    
let build_fmap functions = 
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

    List.fold_left (fun map fdecl -> check_fdecls map fdecl) 
    built_in_decls functions


let check globals functions = 
    let globs = check_globals in
    let fmap = build_fmap functions in
    let sast = convert_ast globs functions fmap 
    in 
    sast 


