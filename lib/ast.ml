type segment =
    | Local
    | Argument
    | This
    | That
    | Constant
    | Static
    | Pointer
    | Temp

(* 'f : function type
   'l : label type  *)
type ('f, 'l) instruction =
    (* stack manipulation *)
    | Push of segment * int
    | Pop of segment * int
    
    (* computations *)
    | Add | Sub | Neg       (* arithmetic *)
    | Eq | Gt | Lt          (* logical *)
    | And | Or | Not        (* bitwise *)

    (* functions *)
    | Function of 'f * int
    | Call of 'f * int
    | Return
    
    (* branching *)
    | Label of 'l
    | Goto of 'l
    | IfGoto of 'l
