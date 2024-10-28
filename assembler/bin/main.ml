(* reads input from `stdin` till EOF (end of file) *)
let rec read_till_eof () =
    let line =
        try read_line () with
        End_of_file -> "\026"
    in
    if line = "\026" then ""
    else line ^ "\n" ^ (read_till_eof ())


(* reading from `stdin` till EOF *)
let input = read_till_eof ()
let tokenized_program = Assembler.Parser.tokenize_program input
let () = Assembler.Encode.pp_avar tokenized_program
