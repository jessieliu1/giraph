(* Authors: 
Jennifer Bi jb3495
Jessie Liu jll2219
*)
open Ast
open Sast

(* Raise an error if the given list has a duplicate *)
let report_duplicate exceptf lst =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
        | _ :: t -> helper t
        | [] -> ()
    in helper (List.sort compare lst)

and report_undeclared_id_assign str = 
    raise (Failure ("assign to undeclared identifier " ^ str))

(* prob will want to change these to actual exceptions *)
and report_bad_binop t1 op t2 = 
    raise (Failure ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2))

and report_bad_unop uop t = 
    raise (Failure ("illegal unary operator " ^
                string_of_uop uop ^ " " ^ string_of_typ t))

and report_bad_assign lt rt = 
    raise (Failure ("illegal assignment " ^ string_of_typ lt ^
                " = " ^ string_of_typ rt))

and check_not_void exceptf = function
    (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()

and report_function_not_found func = 
    raise (Failure ("function " ^ "not found"))

and report_num_args args provided = 
    raise (Failure ("expected " ^ string_of_int args ^ "arguments when " 
        ^ string_of_int provided ^ " arguments were provided"))

and report_typ_args ot nt = 
    raise (Failure ("expected " ^ string_of_typ ot ^ " when " 
        ^ string_of_typ nt ^ " type was provided"))

and report_meth_not_found s2 =
    raise (Failure ("method " ^ s2 ^ " not found"))
and report_concurrent_mod s = 
    raise(Failure("concurrent modification of graph in " ^ s))
