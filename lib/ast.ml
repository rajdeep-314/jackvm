(*
    ast.ml
    
    captures the abstract syntax of the VM using
    variants and records
*)

(* the virtual machine's segments *)
type segment =
    | Local
    | Argument
    | This
    | That
    | Constant
    | Static
    | Pointer
    | Temp

(* 'f -> function name type
   'l -> label name type  *)
type ('f, 'l) inst =
    | Add | Sub | Neg           (* arithmetic *)
    | Eq | Lt | Gt              (* relational *)
    | And | Or | Not            (* bitwise *)

    (* stack manipulation *)
    | Push of segment * int
    | Pop of segment * int

    (* branching *)
    | Label of 'l
    | Goto of 'l
    | IfGoto of 'l

    (* functions *)
    | Call of 'f * int
    | Return

type ('f, 'l) func =
    { name : 'f;
      locals : int;
      body : ('f, 'l) inst list }

(* a VM program is a list of function definitions *)
type ('f, 'l) program = ('f, 'l) func list


type func_name = Fname of string                (* a type for function names *)
type label_name = Lname of string               (* a type for label names    *)


(* module with helper functions to generate VM AST *)
module Helper = struct
    (* operations *)
    let add = Add
    let sub = Sub
    let neg = Neg
    let eq = Eq
    let lt = Lt
    let gt = Gt
    let band = And
    let bor = Or
    let bnot = Not

    (* stack manipulation *)
    let push s n = Push (s, n)
    let pop s n = Pop (s, n)
    let label x = Label (Lname x)
    let goto x = Goto (Lname x)
    let ifgoto x = IfGoto (Lname x)

    (* segments *)
    let local = Local
    let argument = Argument
    let this = This
    let that = That
    let constant = Constant
    let static = Static
    let pointer = Pointer
    let temp = Temp

    (* functions *)
    let call fn n = Call (fn, n)
    let return = Return
end
