type segment =
    | Local
    | Argument
    | This
    | That
    | Constant
    | Static
    | Pointer
    | Temp

type ('f, 'l) instruction =
    | Add | Sub | Neg           (* arithmetic *)
    | Eq | Lt | Gt              (* relational *)
    | And | Or | Not            (* bitwise *)

    | Push of segment * int
    | Pop of segment * int

    | Label of 'l
    | Goto of 'l
    | IfGoto of 'l

    | Function of 'f * int
    | Call of 'f * int
    | Return

type ('f, 'l) program = ('f, 'l) instruction list

type func_name = Fname of string
type label_name = Lname of string

(* 'f -> function name type
   'l -> label name type
   's -> symbol type  *)
type ('f, 'l, 's) asminst = 
    | Flabel of 'f
    | Llabel of 'l
    | Symbol of 's


(* translate : ('f, 'l) program -> ('f, 'l, 's) asminst Assembler.program *)
(* start by testing dependencies *)
