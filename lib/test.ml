(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Ast
open Ast.Helper

let func_body = [
    push constant 42;
    push constant 43;
    eq;
    ifgoto "eq1";
    push constant 34;
    push constant 34;
    eq;
    ifgoto "eq2";
    label "eq1";
    push constant 42;
    return;
    label "eq2";
    push constant 69;
    return;
]

let func = { name = Fname "func"; locals = 0; body = func_body }

let main_body = [
    call (Fname "func") 0;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let myprog = [main; func]

