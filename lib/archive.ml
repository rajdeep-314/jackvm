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

let iter_mul = { name = Fname "iter_mul"; locals = 3; body = iter_mul_body }



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
    call (Fname "rec_mul") 2;
    push argument 1;
    add;
    return;

    label "base_case";
    push constant 0;
    return;
]

let rec_mul = { name = Fname "rec_mul"; locals = 3; body = rec_mul_body }




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



(* recursive fibonacci *)
let rec_fib_body = [
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

let rec_fib = { name = Fname "fib"; locals = 0; body = rec_fib_body }


(* finds the "mid-point" of a number, as in :
        if n mod 2 = 0 then f n = n/2
        else f n = (n - 1)/2  *)
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



(* a messed up multiplication function which works like this :

    to calculate x.y, first calculate p1 and p2 as follows
          p1 = midpoint x
          p2 = x - (midpoint x)           [ evaluated in a very roundabout way ]
    calculate y.p1, using the iterative multiplication function
    calculate y.p2, using the recursive multiplication function
            add y.p1 and y.p2, to get x.y           *)
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


(* recursive factorial function using `megamul`
   as it's multiplication function *)
let rec_fac_body = [
    push argument 0;
    push constant 0;
    eq;
    ifgoto "base_case";

    push argument 0;
    push constant 1;
    sub;
    call (Fname "rec_fac") 1;
    push argument 0;
    call (Fname "megamul") 2;
    return;

    label "base_case";
    push constant 1;
    return;
]

let rec_fac = { name = Fname "rec_fac"; locals = 0; body = rec_fac_body }



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
