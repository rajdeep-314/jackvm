(*
    main.ml

    top-level functions
*)

(* let () = print_endline "Jack Virtual Machine" *)

open Jackvm.Ast
open Jackvm.Ast.Helper
open Jackvm.Translate
open Jackvm.Asm

(* let mulbody = [ *)
(*     push argument 0; *)
(*     pop local 0; *)
(*     push argument 1; *)
(*     pop local 1; *)
(*     push constant 0; *)
(*     pop local 2; *)

(*     label "START"; *)
(*     push local 1; *)
(*     push constant 0; *)
(*     eq; *)
(*     ifgoto "END"; *)
(*     push local 0; *)
(*     push local 2; *)
(*     add; *)
(*     pop local 2; *)
(*     push local 1; *)
(*     push constant 1; *)
(*     sub; *)
(*     pop local 1; *)
(*     goto "START"; *)

(*     label "END"; *)
(*     push local 2; *)
(*     return; *)
(* ] *)

(* let mulfunc = { name = Fname "mul"; locals = 3; body = mulbody } *)

let tfuncbody = [
    push argument 0;
    push argument 1;
    add;
    return;
]

let tfunc = { name = Fname "tfunc"; locals = 3; body = tfuncbody }

let mainbody = [
    push constant 84;
    push constant 30;
    push constant 12;
    call (Fname "tfunc") 2
]

let mainfunc = { name = Fname "Main.main"; locals = 3; body = mainbody }

let myprog = [mainfunc; tfunc]


let asm_prog = translate_prog myprog
let bin = Assembler.Encode.assemble table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

