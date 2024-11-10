(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Ast
open Ast.Helper

(* recursive fibonacci *)
let fib_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "ret0";
    push argument 0;
    push constant 1;
    eq;
    ifgoto "ret1";
    push argument 0;
    push constant 1;
    sub;
    call (Fname "fib") 1;
    push argument 0;
    push constant 2;
    sub;
    call (Fname "fib") 1;
    add;
    return;

    label "ret0";
    push constant 0;
    return;

    label "ret1";
    push constant 1;
    return;
]

let fib = { name = Fname "fib"; locals = 0; body = fib_body }

let main_body = [
    push constant 20;
    call (Fname "fib") 1;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let myprog = [main; fib]

