(*
    main.ml

    top-level functions
*)

(* let () = print_endline "Jack Virtual Machine" *)

open Jackvm.Ast
open Jackvm.Ast.Helper
open Jackvm.Translate
open Jackvm.Asm

(* recursive sum of n nums *)
let tfuncbody = [
    push argument 0;
    push constant 1;
    eq;
    ifgoto "base";

    push argument 0;
    push constant 1;
    sub;
    (* call (Fname "tfunc") 1; *)
    push argument 0;
    push constant 20;
    add;
    add;
    return;

    label "base";
    push constant 1;
    return;
]

let tfunc = { name = Fname "tfunc"; locals = 1; body = tfuncbody }

let mainbody = [
    push constant 1;
    call (Fname "tfunc") 1;
    (* this isn't reached, so maybe it's an issue with tfunc's return (and return in general),
       specifically regarding return going BACK to Main.main's control
       maybe this is what causes the difference due to a change of order as well
       
        
       maybe this is an issue with fcall, fret, their possible duplicates?
       call is what's causing the issue and making the code go back to THAT part
       instead of to Main.main, for some reason
    *)
    push constant 69;
    push constant 42;
    return;
]

let mainfunc = { name = Fname "Main.main"; locals = 0; body = mainbody }

(* let myprog = [tfunc; mainfunc] *)
let myprog = [mainfunc; tfunc]


let asm_prog = translate_prog myprog
let bin = Assembler.Encode.assemble table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

