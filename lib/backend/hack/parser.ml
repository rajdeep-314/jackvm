(*
    parser.ml

    for parsing
*)


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



