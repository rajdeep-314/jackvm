(*
    main.ml

    top-level functions
*)


open Jackvm.Ast.Helper
open Jackvm.Translate
open Jackvm.Asm

let myprog = [
    push constant 10;
    push constant 20;
    push constant 30;
    lt;
    push constant 42;
    push constant 44;
    gt;
    push constant 69;
]

(*
    10
    -1
    0
    69
*)


let asm_prog = translate_prog myprog
let bin = Assembler.Encode.assemble table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

