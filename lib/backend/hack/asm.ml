(*
    asm.ml

    Contains types for the abstract ASM code that the VM
    code will get translated into, and functions related to
    the same
*)

open Jackvm.Ast

(* 'f -> function name type
   'l -> label name type
   's -> symbol type  *)
type ('f, 'l, 's) asminst = 
    | Fcall of 'f                   (* function call label *)
    | Fret of 'f * 'f * int         (* function return label : (callee, caller, line no.) *)
    | Llabel of 'l * 'f             (* label names are local to a function *)
    | Symb of 's                    (* to manage the pre-defined symbols in ASM *)
    | CompSucc of 'f * int
    | CompEnd of 'f * int


(* pre-defined symbols for the assembly code *)
let predefs =
    let ls =
        [("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
        ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
        ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13);
        ("R14", 14); ("R15", 15); ("SCREEN", 16384); ("KBD", 24576);
        ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4)] in
    List.map (fun (x, y) -> (Symb x, y)) ls


(* a hashtable consisting of pre-defined symbols, to be
   passed as an argument to Assembler.Encode.assemble *)
let table : (((func_name, label_name, string) asminst, int) Hashtbl.t)=
    Hashtbl.of_seq (List.to_seq predefs)

