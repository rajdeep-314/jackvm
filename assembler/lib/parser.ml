(*
    parser.ml

    This is very basic and only for testing the accuracy of the assembler
    It has no use for the VM compilation
    Most of it is the previous assembler's parser

    THIS PARSER IS NOT INTENDED TO GIVE ANY ERRORS
*)


open Ast


(* slices `input` from index `start` to the end *)
let slice_string (input : string) (start : int) : string =
    if String.length input - 1 < start then "" else
    String.sub input start ((String.length input) - start)


(* strips `input` of whitespaces (' ' and '\t) on
   either of it's sides  *)
let rec remove_whitespaces (input : string) =
    if (String.starts_with ~prefix:" " input) || (String.starts_with ~prefix:"\t" input) then
        remove_whitespaces (slice_string input 1)
    else if (String.ends_with ~suffix:" " input) || (String.ends_with ~suffix:"\t" input) then
        remove_whitespaces (String.sub input 0 ((String.length input) - 1))
    else
        input
        

(* takes a line and remove the first occurrence of
   "//" and everything after that  *)
let rec remove_comment (input : string) : string =
    if input = "" then ""
    else if String.starts_with ~prefix:"//" input then ""
    else String.sub input 0 1 ^ (remove_comment (slice_string input 1))


(* returns a list of non empty lines from `input` *)
let rec remove_empty_lines (input : (string list)) : (string list) =
    match input with
    [] -> []
    | head :: tail -> let post = remove_empty_lines tail in
                        if head = "" then post
                        else head :: post


(* splits the program into lines and removes
   leading and trailing whitespaces (' ', '\t')  *)
let get_lines (program : string) : (string list) =
    let lines = String.split_on_char '\n' program in
    let lines_wo_comments = List.map remove_comment lines in
    let stripped_lines = List.map remove_whitespaces lines_wo_comments in
    remove_empty_lines stripped_lines


let lowercase_alphabets = "abcdefghijklmnopqrstuvwxyz"
let uppercase_alphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let alphabets = lowercase_alphabets ^ uppercase_alphabets
let digits = "0123456789"
let special_characters = ":$._"

let valid_pvar_begin = special_characters ^ alphabets
let valid_pvar_post = valid_pvar_begin ^ digits

(* helper function for `is_pvar' below *)
let rec is_pvar_post (input : string) : bool =
    if input = "" then true
    else
        String.contains valid_pvar_post input.[0] &&
        (is_pvar_post (slice_string input 1))

(* returns if `input` is a valid program variable name *)
let is_pvar (input : string) : bool =
    if input = "" then false
    else
        String.contains valid_pvar_begin input.[0] &&
        (is_pvar_post (slice_string input 1))
    

(* checks if `input` only has numerical characters
   for implementation purposes, "" is a valid digit  *)
let rec is_digit (input : string) : bool =
    if input = "" then true
    else
        String.contains digits input.[0] &&
        (is_digit (slice_string input 1))


(* Ldef instruction validation *)
let validate_ldef (input : string) : bool =
    if String.length input < 3 then false
    else
        input.[0] = '(' &&
        (input.[((String.length input) - 1)] = ')') &&
        (is_pvar (String.sub input 1 ((String.length input) - 2)))


(* At instruction validation *)
let validate_at (input : string) : bool =
    if String.length input < 2 then false
    else
        input.[0] = '@' &&
        (is_digit (slice_string input 1))


(* Ainst validation *)
let validate_ainst (input : string) : bool =
    if String.length input < 2 then false
    else
        input.[0] = '@' &&
        (is_pvar (slice_string input 1))


(* Cinst validation *)
(* C instructions have to be of the form
            dest=comp;jump
   but either (or both) of dest and jump can be
   ommitted from the input. We first transform each
   C instruction to the standard form  *)
let rec make_standard (input : string) : string =
    if not (String.contains input ';') then
        make_standard (input ^ ";NullJump")
    else if not (String.contains input '=') then
        make_standard ("NullDest=" ^ input)
    else
        input


(* Valid strings for a jump sub-instruction and a function to check the same *)
let valid_jump = ["NullJump"; "JGT"; "JEQ"; "JGE"; "JLT"; "JNE"; "JLE"; "JMP"]
let validate_jump jump = List.mem jump valid_jump

(* Valid strings for a dest sub-instruction and the corresponding function *)
let valid_dest = ["NullDest"; "M"; "D"; "DM"; "A"; "AM"; "AD"; "ADM"; "MD"; "AMD"]
let validate_dest dest = List.mem dest valid_dest

(* Valid strings for a comp sub-instruction and the corresponding function *)
let valid_comp = ["0"; "1"; "-1"; "D"; "A"; "!D"; "!A"; "-D"; "-A"; "D+1"; "A+1"; "D-1"; "A-1"; "D+A";
                  "D-A"; "A-D"; "D&A"; "D|A"; "M"; "!M"; "-M"; "M+1"; "M-1"; "D+M"; "D-M"; "M-D"; "D&M"; "D|M"]
let validate_comp comp = List.mem comp valid_comp


(* TOKENIZATION *)


(* tokenizes a Ldef string *)
let tokenize_ldef (input : string) : avar inst =
    Ldef (Symbol (String.sub input 1 ((String.length input) - 2)))

(* tokenizes a At string *)
let tokenize_at (input : string) : avar inst =
    At (int_of_string (slice_string input 1))

(* tokenizes a Ainst string *)
let tokenize_ainst (input : string) : avar inst =
    Ainst (Symbol (slice_string input 1))


(* tokenizes a destination string *)
let tokenize_dest = function
    | "NullDest" -> []
    | "M" -> [M] | "D" -> [D] | "DM" -> [D;M] | "A" -> [A] | "AM" -> [A;M] | "AD" -> [A;D] | "ADM" -> [A;D;M]
    | "MD" -> [M;D] | "AMD" -> [A;M;D]
    | _ -> []


(* tokenizes a jump string *)
let tokenize_jump = function
    | "JGT" -> JGT | "JEQ" -> JEQ | "JGE" -> JGE | "JLT" -> JLT
    | "JNE" -> JNE | "JLE" -> JLE | "JMP" -> JMP
    | _ -> NullJump


(* helper functions to generate computations for abstract syntax *)
let id x = UnaryOp (Identity, x)
let bnot x = UnaryOp (BNot, x)
let neg x = UnaryOp (Neg, x)
let succ x = UnaryOp (Succ, x)
let pred x = UnaryOp (Pred, x)
let add x = BinaryOp (Add, x)
let sub x = BinaryOp (Sub, x)
let subfrom x = BinaryOp (SubFrom, x)
let band x = BinaryOp (BAnd, x)
let bor x = BinaryOp (BOr, x)

(* tokenizes a computation string *)
let tokenize_comp = function
    | "0" -> Const Zero | "1" -> Const One | "-1" -> Const MinusOne
    | "D" -> id D | "M" -> id M | "A" -> id A
    | "!D" -> bnot D | "!M" -> bnot M | "!A" -> bnot A
    | "-D" -> neg D | "-M" -> neg M | "-A" -> neg A
    | "D+1" -> succ D | "M+1" -> succ M | "A+1" -> succ A
    | "D-1" -> pred D | "M-1" -> pred M | "A-1" -> pred A
    | "D+M" -> add M | "M+D" -> add M | "D+A" -> add A | "A+D" -> add A
    | "D-M" -> sub M | "D-A" -> sub A
    | "M-D" -> subfrom M | "A-D" -> subfrom A
    | "D&M" -> band M | "M&D" -> band M | "D&A" -> band A | "A&D" -> band A
    | "D|M" -> bor M | "M|D" -> bor M | "D|A" -> bor A | "A|D" -> bor A
    | _ -> Const Zero


(* tokenizes a Cinst string, making use of the 
   helper functions above  *)
let tokenize_cinst (input : string) : avar inst =
    let standard = make_standard input in
    let x = String.split_on_char ';' standard in
    let y = String.split_on_char '=' (List.nth x 0) in
    let dest = List.nth y 0 in
    let comp = List.nth y 1 in
    let jump = List.nth x 1 in
    let dest_token = tokenize_dest dest in
    let comp_token = tokenize_comp comp in
    let jump_token = tokenize_jump jump in
    Cinst (dest_token, comp_token, jump_token)
    

(* tokenizes a string after validating it.
   if the string is invalid, it raises an exception  *)
let tokenize_instruction (input : string) : avar inst =
    if validate_ldef input then
        tokenize_ldef input
    else if validate_at input then
        tokenize_at input
    else if validate_ainst input then
        tokenize_ainst input
    else
        tokenize_cinst input

let tokenize_program (input : string) : (avar program) =
    let lines = get_lines input in
    List.map tokenize_instruction lines
