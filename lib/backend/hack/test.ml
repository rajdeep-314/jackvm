(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Jackvm.Ast
open Helper

let func_body = [
    push argument 0;
    push argument 1;
    add;
    return;
]

let func = { name = Fname "func"; locals = 0; body = func_body }

let func2_body = [
    push argument 0;
    push argument 1;
    push argument 0;
    push argument 1;
    add;
    add;
    add;
    return;
]

let func2 = { name = Fname "func"; locals = 0; body = func2_body }

let main_body = [
    push constant 42;
    push constant 24;
    call (Fname "func") 2;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let myprog = [func2; func; main]
