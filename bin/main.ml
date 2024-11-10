(*
    main.ml

    top-level functions
*)

(* let () = print_endline "Jack Virtual Machine" *)


let asm_prog = Jackvm.Translate.translate_prog Jackvm.Test.myprog
let bin = Assembler.Encode.assemble Jackvm.Asm.table 0 asm_prog

let () = Assembler.Encode.pp_bin bin

