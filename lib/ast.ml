(*
    ast.ml
    
    captures the abstract syntax of the VM using
    variants and records
*)

type segment =
    | Local
    | Argument
    | This
    | That
    | Constant
    | Static
    | Pointer
    | Temp

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
      args : int;
      body : ('f, 'l) inst list }

type ('f, 'l) program = ('f, 'l) func list

module Helper = struct
    let push s n = Push (s, n)
    let pop s n = Pop (s, n)
    let add = Add
    let sub = Sub
    let neg = Neg
    let eq = Eq
    let lt = Lt
    let gt = Gt
    let band = And
    let bor = Or
    let bnot = Not
    let label x = Label x
    let goto x = Goto x
    let ifgoto x = IfGoto x

    (* segments *)
    let local = Local
    let argument = Argument
    let this = This
    let that = That
    let constant = Constant
    let static = Static
    let pointer = Pointer
    let temp = Temp
end
