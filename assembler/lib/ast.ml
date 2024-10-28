(*
    ast.ml

    Captures the abstract syntax of HACK Assembly
*)

type address = int                  (* a type alias for numerical addresses *)

type register = A | M | D
type dest = register list

type const = Zero | One | MinusOne
type unary_op = Identity | BNot | Neg | Succ | Pred
type binary_op = Add | Sub | SubFrom | BAnd | BOr

type comp =
    | Const of const
    | UnaryOp of unary_op * register
    | BinaryOp of binary_op * register

type jump = JEQ | JNE | JLE | JLT | JGE | JGT | JMP | NullJump

(* 'v : a type for a virtual address *)
type 'v inst =
    | Ldef of 'v
    | At of address
    | Ainst of 'v
    | Cinst of dest * comp * jump

type 'v table = ('v, address) Hashtbl.t
type 'v program = 'v inst list


type avar = Symbol of string        (* 'v specification *)



(* updates `symbol_table` with labels from `prog`, starting
   from the line number `curr_address`
   this is the first pass of the assembler
   get_labels : 'v inst list -> 'v table -> address -> 'a list  *)
let rec get_labels (prog : 'v inst list) (symbol_table : 'v table) (curr_address : int) =
    match prog with
    | [] -> []
    | (Ldef label_name) :: tail ->
            let () = Hashtbl.add symbol_table label_name curr_address in
            get_labels tail symbol_table curr_address

    | _ :: tail ->
            get_labels tail symbol_table (curr_address + 1)

(* resolves a `v symbol to an integer address,
   and returns the same, along with the updated `curr_address`
   resolve_symbol : 'v -> 'v table -> address -> address * address  *)
let resolve_symbol (symbol : 'v) (symbol_table : 'v table) (curr_address : address) =
    if Hashtbl.mem symbol_table symbol then
        (Hashtbl.find symbol_table symbol, curr_address)
    else
        let () = Hashtbl.add symbol_table symbol curr_address in
        (curr_address, curr_address + 1)

