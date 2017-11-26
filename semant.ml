(* Semantic checking for the MicroC compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.f_name) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.f_name) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { f_typ = Void; f_name = "print"; f_formals = [(Int, "x")];
       f_body = [] } (StringMap.add "printb"
     { f_typ = Void; f_name = "printb"; f_formals = [(Bool, "x")];
       f_body = [] } (StringMap.singleton "printbig"
     { f_typ = Void; f_name = "printbig"; f_formals = [(Int, "x")];
       f_body = [] }))
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.f_name fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.f_name)) func.f_formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.f_name)
      (List.map snd func.f_formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.f_name)) func.f_locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.f_locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.f_formals @ A.)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	Int_Lit _ -> Int
      | Bool_Lit _ -> Bool
      | Float_Lit _ -> Float
      | String_Lit _ -> String
      | Node _ -> Node
      | Edge _ -> Edge 
      | Graph _ -> Graph
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	(match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int (*can remove guard? *) 
        | Mod when t1 = Int && t2 = Int -> Int 
        | Eq | Neq | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
	| And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(f_name, actuals) as call -> let fd = function_decl f_name in
         if List.length actuals != List.length fd.f_formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.f_formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.f_formals actuals;
           fd.f_typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* not implemented *)
    (*let rec check_loop = function _ -> () *)
    
    (* Verify a statement or throw an exception *)
    let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.f_typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.f_typ ^ " in " ^ string_of_expr e))
      | Vdecl (t, n, e) -> ignore (type_of_identifier n)
      | Break b2 -> ()(* check within a loop *)
      | Continue b2 -> () (* check within a loop *)
      | For_Node(e1, e2, st) -> () (* not implemented *)
      | For_Edge (e1, e2, st) -> () (* not implemented *)
      | Bfs (e1, e2, e3, st) -> () (*not impl *)
      | Dfs(e1, e2, e3, st) -> () (* not impl *)
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.f_body)
   
  in
  List.iter check_function functions
