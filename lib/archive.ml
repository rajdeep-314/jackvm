(*
    archive.ml

    keeps record of a bunch of functions for frequent
    use in testing
*)

open Ast
open Ast.Helper

(* iterative multiplication function *)
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

let iter_mul = { name = Fname "mul"; locals = 3; body = iter_mul_body }



(* recursive multiplication function *)
let rec_mul_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "base_case";
    
    push argument 0;
    push constant 1;
    sub;
    push argument 1;
    call (Fname "mul") 2;
    push argument 1;
    add;
    return;

    label "base_case";
    push constant 0;
    return;
]

let rec_mul = { name = Fname "mul"; locals = 3; body = rec_mul_body }




(* iterative fibonacci *)
let iter_fib_body = [
    push constant 0;
    pop local 0;
    push constant 1;
    pop local 1;
    push argument 0;
    pop local 2;

    label "START";
    push local 2;
    push constant 0;
    eq;
    ifgoto "END";
    push local 0;
    push local 1;
    add;
    pop local 1;
    push local 1;
    push local 0;
    sub;
    pop local 0;
    push local 2;
    push constant 1;
    sub;
    pop local 2;
    goto "START";

    label "END";
    push local 0;
    return;
]

let iter_fib = { name = Fname "fib"; locals = 3; body = iter_fib_body }
