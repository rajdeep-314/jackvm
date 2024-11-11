(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Ast
open Ast.Helper

(* a mod b *)
let modulo_body = [
    push argument 0;
    push argument 1;
    lt;
    ifgoto "base_case";

    push argument 0;
    push argument 1;
    sub;
    push argument 1;
    call (Fname "modulo") 2;
    return;

    label "base_case";
    push argument 0;
    return;
]

let modulo = { name = Fname "modulo"; locals = 0; body = modulo_body }

(* floor of a/b *)
let intdiv_body = [
    push argument 0;
    push argument 1;
    lt;
    ifgoto "base_case";

    push argument 0;
    push argument 1;
    sub;
    push argument 1;
    call (Fname "intdiv") 2;
    push constant 1;
    add;
    return;
    
    label "base_case";
    push constant 0;
    return;
]

let intdiv = { name = Fname "intdiv"; locals = 0; body = intdiv_body }

let main_body = [
    push constant 107;
    push constant 7;
    call (Fname "modulo") 2;
    push constant 107;
    push constant 7;
    call (Fname "intdiv") 2;
    add;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let myprog = [modulo; main; intdiv]
