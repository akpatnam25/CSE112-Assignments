(* $Id: interp.ml,v 1.8 2020-01-24 11:42:24-08 - - $ *)
(* Sasank Madineni (smadinen) *)
(* Henry Nguyen (hnguye87) *)

open Absyn
open Printf

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let getname data = match data with
		| Arrayref (ident, expr) -> no_expr "array-ref"
  	| Variable ident -> Printf.printf "%s\n" ident ; ident

let rec eval_expr (expr : Absyn.expr) : float =
		(match expr with
  	| Number number -> number
		| Memref (memref : Absyn.memref) -> (match memref with
				| Arrayref (ident, expr) -> let loc = int_of_float (eval_expr expr) in
						let val_array = Hashtbl.find Tables.array_table ident in
								val_array.(loc)
				| Variable ident -> Hashtbl.find Tables.variable_table ident
				)
		| Unary (oper, expr) -> let value = eval_expr expr in
				Hashtbl.find Tables.unary_fn_table oper value
		| Binary (oper, expr1, expr2) -> let value1 = eval_expr expr1 in
				let value2 = eval_expr expr2 in
	  				Hashtbl.find Tables.binary_fn_table oper value1 value2
		)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)


and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continuation
    | Let (memref, expr) -> interp_let memref expr continuation
    | Goto label ->  interp_goto label continuation
    | If (expr, label) -> interp_if expr label continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation

(*
* function: interp_if
* description: if <expr> is true, then go to <label>
* paramters:
*)
and interp_if (expr) (label) (continuation) =
		match expr with
		| Binary (oper, expr1, expr2) -> let val1 = eval_expr expr1 in
				let val2 = eval_expr expr2 in
						if (Hashtbl.find Tables.boolean_fn_table oper val1 val2) = true then
								interp_goto label continuation
						else
								interpret continuation
		| Unary ( oper, expr) -> no_expr
	  		"Invalid If Statement, Given Unary Operator."
		| Memref memref -> no_expr
				"Invalid If Statement, Given Memory Reference."
	  | Number number -> no_expr
	      "Invalid If Statement, Given Number as Conditional."

and interp_goto (label) (continuation) =
	  interpret (Hashtbl.find Tables.label_table label);
	  (*No interpret continuation to transfer full control*)

and interp_dim (ident) (expr) (continuation) =
	  Hashtbl.add Tables.array_table (ident)
	    	(Array.make (int_of_float(eval_expr(expr))) 0.0);
	  interpret continuation

and interp_let (memref) (expr1) (continuation) =
	  (match memref with
	  | Arrayref (ident, expr) ->
		let loc = int_of_float (eval_expr expr) in
		  	let val_array = Hashtbl.find Tables.array_table ident in
			  		val_array.(loc) <- (eval_expr expr1);
						(* print_string "Checking value in let: ";
						print_float (val_array.(loc));
						print_newline (); *)
	  | Variable ident ->
				Hashtbl.add Tables.variable_table ident (eval_expr expr1)
		);
	  interpret continuation

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr)
				 )
    in (List.iter print_item print_list; print_newline ());
    interpret continuation


and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
    let input_number memref =
        try let number = Etc.read_number ()
             in match memref with
		| Variable ident -> Hashtbl.add Tables.variable_table ident number
				(* with End_of_file ->
						(no_expr "Stop program with eof") *)
		| _ -> no_expr "not possible"
        with End_of_file ->
             (Etc.usage_exit ["Exiting Program"])
    in List.iter input_number memref_list;
    interpret continuation


let interpret_program program =
    (Tables.init_label_table program;
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
