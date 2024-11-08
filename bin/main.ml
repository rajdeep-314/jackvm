(*
    main.ml

    top-level functions
*)


open Jackvm.Ast.Helper
open Jackvm.Translate
open Jackvm.Asm

let myprog = [
    push constant 0;
    push constant 1;
    push constant 200;
    pop local 2;
    pop local 1;
    pop local 0;

    label "start";
    push local 2;
    push constant 0;
    eq;
    ifgoto "end";
    push local 0;
    push local 1;
    add;
    pop local 0;
    push local 1;
    push constant 1;
    add;
    pop local 1;
    push local 2;
    push constant 1;
    sub;
    pop local 2;
    goto "start";
    
    label "end";
    push local 0;
    pop static 0;
]



let asm_prog = translate_prog myprog
let bin = Assembler.Encode.assemble table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

