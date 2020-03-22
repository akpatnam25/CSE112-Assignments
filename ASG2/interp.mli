(* $Id: interp.mli,v 1.7 2020-01-24 12:57:06-08 - - $ *)
(* Sasank Madineni (smadinen) *)
(* Henry Nguyen (hnguye87) *)
(*
* Interpreter for Silly Basic
*)

val want_dump : bool ref

val interpret_program : Absyn.program -> unit
