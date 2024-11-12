(*
    main.ml

    top-level functions
*)


let asm_prog = Hack.Translate.translate_prog Hack.Test.myprog
let bin = Assembler.Encode.assemble Hack.Asm.table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

