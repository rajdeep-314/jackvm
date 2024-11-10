(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Ast
open Ast.Helper

let midpoint_body = [
    push argument 0;
    push constant 2;
    lt;
    ifgoto "base";
    
    push argument 0;
    push constant 2;
    sub;
    call (Fname "midpoint") 1;
    push constant 1;
    add;
    return;

    label "base";
    push constant 0;
    return;
]

let midpoint = { name = Fname "midpoint"; locals = 0; body = midpoint_body }

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

let rec_mul_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "END";
    
    push argument 0;
    push constant 1;
    sub;
    push argument 1;
    call (Fname "rec_mul") 2;
    push argument 1;
    add;
    return;

    label "END";
    push constant 0;
    return;
]

let rec_mul = { name = Fname "rec_mul"; locals = 3; body = rec_mul_body }

let megamul_body = [
    push argument 0;
    push argument 1;
    pop local 1;
    pop local 0;
    push local 0;
    call (Fname "midpoint") 1;
    pop local 2;
    push local 2;
    push local 2;
    add;
    push local 0;
    eq;
    ifgoto "valid";
    push local 2;
    push constant 1;
    add;
    pop local 3;
    goto "endvalidation";

    label "valid";
    push local 2;
    pop local 3;

    label "endvalidation";

    push local 1;
    push local 2;
    call (Fname "iter_mul") 2;
    push local 1;
    push local 3;
    call (Fname "rec_mul") 2;
    add;
    return;
]

let megamul = { name = Fname "megamul"; locals = 10; body = megamul_body }

let rec_fac_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "END";
    push argument 0;
    push constant 1;
    sub;
    call (Fname "rec_fac") 1;
    push argument 0;
    call (Fname "megamul") 2;
    return;

    label "END";
    push constant 1;
    return;
]

let rec_fac = { name = Fname "rec_fac"; locals = 0; body = rec_fac_body }

let main_body = [
    push constant 7;
    call (Fname "rec_fac") 1;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let myprog = [midpoint; main; rec_fac; rec_mul; iter_mul; megamul]
