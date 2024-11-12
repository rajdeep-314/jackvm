(*
    main.ml

    manages the multiple .vm input files, parses them,
    compiles them to .asm code, and bootstraps and links
    everything together
*)


(* reads lines from in_channel and returns a string *)
let read_lines fname =
    let lines = ref [] in
    try
        while true; do
        lines := input_line fname :: !lines
        done
    with End_of_file ->
        close_in fname;
        String.concat "\n" (List.rev !lines)

(* extracts the 'tail' of a list *)
let tail = function
    | [] -> []
    | _ :: tail -> tail

let () =
    (* list of file names *)
    let files = tail (Array.to_list Sys.argv) in
    (* in_channels of files *)
    let channels = List.map open_in files in
    let text_progs = List.map read_lines channels in
    (* parsing text programs to get AST programs *)
    let programs = List.map Jackvm.Parser.tokenize_file text_progs in
    (* translating each program to ASM AST *)
    let assembly_progs = List.map2 (fun x y -> Hack.Translate.translate_prog x y) files programs in
    (* concatenating all ASM programs into one *)
    let combined_asm_prog = List.concat assembly_progs in
    (* pre-appending the bootstrap ASM code *)
    let final_prog = Hack.Bootstrap.bootstrap_asm @ combined_asm_prog in
    (* assembling to binary using the Assembler library *)
    let bin = Assembler.Encode.assemble Hack.Asm.table 16 final_prog in
    (* printing 16-character binary strings to stdout *)
    Assembler.Encode.pp_bin bin

