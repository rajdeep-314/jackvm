(*
    encode.ml

    Contains functions to encode HACK ASM's abstract instructions to
    16-character binary strings
*)


open Ast


(* encoding C-instructions *)

let encode_const = function
    | Zero -> "0101010"
    | One -> "0111111"
    | MinusOne -> "0111010"

let encode_unaryop = function
    | Identity -> "00"
    | BNot -> "01"
    | Neg -> "11"
    | Succ -> "11"      (* not used *)
    | Pred -> "10"

let encode_reg_unary = function
    | D -> "00011"
    | A -> "01100"
    | M -> "11100"

let encode_reg_binary = function
    | M -> "1"
    | A -> "0"
    | D -> "0"          (* not used *)

let encode_binaryop = function
    | Add -> "000010"
    | Sub -> "010011"
    | SubFrom -> "000111"
    | BAnd -> "000000"
    | BOr -> "010101"

let encode_succ = function
    | D -> "0011111"
    | A -> "0110111"
    | M -> "1110111"

let encode_comp = function
    | Const constant -> encode_const constant
    | UnaryOp (Succ, reg) -> encode_succ reg 
    | UnaryOp (op, reg) -> encode_reg_unary reg ^ (encode_unaryop op)
    | BinaryOp (op, reg) -> encode_reg_binary reg ^ (encode_binaryop op)

let encode_jump = function
    | NullJump -> "000"
    | JGT -> "001"
    | JEQ -> "010"
    | JGE -> "011"
    | JLT -> "100"
    | JNE -> "101"
    | JLE -> "110"
    | JMP -> "111"


(* to "add" strings, in the sense that
        "100" + "010" = "110"       *)
let string_sum str1 str2 = string_of_int (int_of_string str1 + (int_of_string str2))

(* similar to Python's str.zfill - fills in zeros
   to the left till the string length is equal to n  *)
let rec zfill string n =
    if (String.length string >= n)
        then (String.sub string ((String.length string) - n) n)
    else
        "0" ^ (zfill string (n-1))

let encode_register = function
    | A -> "100"
    | D -> "010"
    | M -> "001"

let encode_dest (input : dest) =
    let encoded_registers = List.map encode_register input in
    let string_output = List.fold_left string_sum "0" encoded_registers in
    zfill string_output 3

let encode_cinst (dest, comp, jump) =
    "111" ^ encode_comp comp ^ (encode_dest dest) ^ (encode_jump jump)



(* encoding A-instructions *)

let rec dec_to_bin number =
    if number = 0 then 0
    else if (number mod 2 = 1) then dec_to_bin (number - 1) + 1
    else 10 * (dec_to_bin (number/2))

(* encodes @address *)
let encode_at (address : address) =
    let binary_address = zfill (string_of_int (dec_to_bin address)) 15 in
    "0" ^ binary_address


let rec encode_program (prog : 'v program) (symb_table : 'v table) (symb_address : address) =
    match prog with
    | [] -> []
    | Ldef _ :: tail ->
            encode_program tail symb_table symb_address
    | At address :: tail ->
            encode_at address :: encode_program tail symb_table symb_address
    | Ainst symbol :: tail ->
            let resolved_address, new_address = resolve_symbol symbol symb_table symb_address in
            encode_at resolved_address :: (encode_program tail symb_table new_address)
    | Cinst (dest, comp, jump) :: tail ->
            let encoded = encode_cinst (dest, comp, jump) in
            encoded :: (encode_program tail symb_table symb_address)

(* prog -> the program
   presets -> hashtable with predefined symbols
   symb_address -> address from where the program variable allocation begins  *)
let assemble (presets : 'v table) (symb_address : address) (prog : 'v program) : (string list) =
(* let assemble_helper (prog : 'v program) (presets : 'v table) (symb_address : address) : (string list) = *)
    let symb_table = presets in
    let _ = get_labels prog symb_table 0 in
    encode_program prog symb_table symb_address
    
let pp_bin (lines : string list) =
    let lines = String.concat "\n" lines in
    print_endline lines


(* i should probably put these functions in some other file *)

(* implementation specific functions from here on *)

(* an 'avar table' with symbols for pre-defined symbols *)
let avar_presets =
    let string_presets = [
        ("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
        ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
        ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13);
        ("R14", 14); ("R15", 15); ("SCREEN", 16384); ("KBD", 24576);
        ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4)] in
    let symb_presets = List.map (fun (x, y) -> (Symbol x, y)) string_presets in
    let presets_seq = List.to_seq symb_presets in
    Hashtbl.of_seq presets_seq

(* assemble_avar : avar program -> string list *)
let assemble_avar (prog : avar program) : (string list) =
    assemble avar_presets 16 prog

let pp_avar (prog : 'v program) =
    let assembled = assemble_avar prog in
    let output = String.concat "\n" assembled in
    print_endline output
