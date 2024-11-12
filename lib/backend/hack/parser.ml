(*
    parser.ml

    for parsing
*)

open Jackvm.Ast
open Helper

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

let valid_identifier_begin = special_characters ^ alphabets
let valid_identifier_post = valid_identifier_begin ^ digits


(* helper function for `is_identifier' below *)
let rec is_identifier_post (input : string) : bool =
    if input = "" then true
    else
        String.contains valid_identifier_post input.[0] &&
        (is_identifier_post (slice_string input 1))

(* returns if `input` is a valid program variable name *)
let is_identifier (input : string) : bool =
    if input = "" then false
    else
        String.contains valid_identifier_begin input.[0] &&
        (is_identifier_post (slice_string input 1))

let rec is_num (input : string) : bool =
    if  input = "" then true
    else if String.contains digits input.[0] then
        is_num (String.sub input 1 (String.length input - 1))
    else false


(* validation functions *)

(* validates operations - arithmetic, bitwise, relational *)
let validate_op input =
    List.mem input ["add"; "sub"; "neg"; "and"; "or"; "not"; "lt"; "gt"; "eq"]

let is_segment input =
    List.mem input ["constant"; "this"; "that"; "argument"; "local"; "temp"; "pointer"; "static"]

(* validates a stack manipulation instruction *)
let validate_stackmanip input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 3 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        let p3 = List.nth parts 2 in
        List.mem p1 ["push"; "pop"] &&
        is_segment p2 &&
        is_num p3

let validate_funcdef input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 3 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        let p3 = List.nth parts 2 in
        List.length parts = 3 && p1 = "function" && is_identifier p2 && is_num p3

let validate_call input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 3 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        let p3 = List.nth parts 2 in
        p1 = "call" && is_identifier p2 && is_num p3

let validate_return input = input = "return"

let validate_label input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 2 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        p1 = "label" && is_identifier p2

let validate_goto input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 2 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        p1 = "goto" && is_identifier p2

let validate_ifgoto input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    if List.length parts != 2 then false
    else
        let p1 = List.nth parts 0 in
        let p2 = List.nth parts 1 in
        p1 = "if-goto" && is_identifier p2


(* tokenization functions *)

let tokenize_op  = function
    | "add" -> add | "sub" -> sub | "neg" -> neg
    | "and" -> band | "or" -> bor | "not" -> bnot
    | "lt" -> lt | "gt" -> gt | _ -> eq

let tokenize_stackmanip input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    let p1 = ( match List.nth parts 0 with
        | "push" -> push
        | _ -> pop) in
    let p2 = ( match List.nth parts 1 with
        | "constant" -> constant | "static" -> static | "this" -> this | "that" -> that 
        | "temp" -> temp | "argument" -> argument | "local" -> local | _ -> pointer ) in
    let p3 = int_of_string (List.nth parts 2) in
    p1 p2 p3

let tokenize_call input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    call (Fname (List.nth parts 1)) (int_of_string (List.nth parts 2))

let tokenize_return = return

let tokenize_label input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    label (List.nth parts 1)

let tokenize_goto input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    goto (List.nth parts 1)

let tokenize_ifgoto input =
    let parts = remove_empty_lines (String.split_on_char ' ' input) in
    ifgoto (List.nth parts 1)


let tokenize_inst input =
    if validate_op input
        then tokenize_op input
    else if validate_stackmanip input
        then tokenize_stackmanip input
    else if validate_call input 
        then tokenize_call input
    else if validate_return input
        then tokenize_return
    else if validate_label input
        then tokenize_label input
    else if validate_goto input
        then tokenize_goto input
    else if validate_ifgoto input
        then tokenize_ifgoto input
    else goto ""


(* tokenizes a list of strings *)
let rec tokenize_program input prev =
    match input with
    | [] -> prev
    | curr_line :: tail ->
        if validate_funcdef curr_line then
            (* let () = print_endline ("Validated as function: " ^ curr_line) in *)
            let parts = remove_empty_lines (String.split_on_char ' ' curr_line) in
            let fname = Fname (List.nth parts 1) in
            let nlocs = int_of_string (List.nth parts 2) in
            tokenize_program tail (prev @ [{ name = fname; locals = nlocs; body = [] }])
        else
            (* let () = print_endline ("current inst: " ^ curr_line ) in *)
            let tok = tokenize_inst curr_line in
            ( match List.rev prev with
            | [] -> failwith "Parsing error: the program must be a bunch of function definitions"
            | { name = fname; locals = nlocs; body = bd } :: ls ->
                    let updated_rev = { name = fname; locals = nlocs; body = bd @ [tok] } :: ls in
                    tokenize_program tail (List.rev updated_rev) )
                

(* applies tokenize_program after applying get_lines *)
let tokenize_file input =
    let lines = get_lines input in
    tokenize_program lines []
