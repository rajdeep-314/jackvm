(*
    test.ml

    To test the VM translator using helper functions from
    Ast.Helper, before the parser is written
*)

open Jackvm.Ast
open Helper

let f1 = 
    { name = Fname "f1"; locals = 0;
    body = [
        push argument 0;
        pop static 10;
        push constant 0;
        return;
    ] }

let f2 =
    { name = Fname "f2"; locals = 0;
    body = [
        push static 10;
        push argument 0;
        add;
        return;
    ] }

let main_body = [
    push constant 42;
    call (Fname "f1") 1;
    push constant 24;
    call (Fname "f2") 1;
    return;
]

let main = { name = Fname "Main.main"; locals = 0; body = main_body }

let sysinit = 
    { name = Fname "Sys.init"; locals = 0;
    body = [
        call (Fname "Main.main") 0;
        label "INF";
        goto "INF"
    ] }

let myprog = [sysinit; main; f1; f2]
