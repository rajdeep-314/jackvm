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
