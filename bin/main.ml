(*
    main.ml

    top-level functions
*)


let asm_prog = Hack.Translate.translate_prog "test.vm" Hack.Test.myprog
let bin = Assembler.Encode.assemble Hack.Asm.table 16 asm_prog

let () = Assembler.Encode.pp_bin bin

