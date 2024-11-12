(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Jackvm.Ast
open Helper

let iter_mul_body = [
    push argument 0;
    pop local 0;
    push argument 1;
    pop local 1;
    push constant 0;
    pop local 2;

    label "START";
    push local 1;
    push constant 0;
    eq;
    ifgoto "END";
    push local 0;
    push local 2;
    add;
    pop local 2;
    push local 1;
    push constant 1;
    sub;
    pop local 1;
    goto "START";

    label "END";
    push local 2;
    return;
]

let iter_mul = { name = Fname "iter_mul"; locals = 3; body = iter_mul_body }

let main_body = [
    push constant 7;
    call (Fname "fac") 1;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let fac_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "base_case";

    push argument 0;
    push constant 1;
    sub;
    call (Fname "fac") 1;
    push argument 0;
    call (Fname "iter_mul") 2;
    return;

    label "base_case";
    push constant 1;
    return;
]

let fac = { name = Fname "fac"; locals = 0; body = fac_body }

let myprog = [iter_mul; main; fac]

