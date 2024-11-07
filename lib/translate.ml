(*
    translate.ml
    
    contains functions to translate VM code to it's corresponding
    ASM code
*)


open Ast
open Asm
open Subroutines
open Assembler.Ast.Helper


(* arithmetic, relational and bitwise *)

(* list of sub-routines for the translation of
   the binary operation `op`
   eg : add, sub, bitwise and  *)
let binop_routines op = [
    load_top;
    goto_top;
    [assign d op];
    goto_top;
    [assign m (iden dreg)] ]

(* list of sub-routines for the translation of
   the unary operation `op`
   eg : neg, bitwise not  *)
let unop_routines op = [
    goto_top;
    [assign m op] ]

(* list of sub-routines for the translation of
   the comparison operation `op`
   eg : eq, lt, gt  *)
let comp_routines n op = [
    load_top;
    goto_top;
    [
        assign d (minusd mreg);
        ldef (CompSucc n);
        djump op;
        assign m (zero);
        ainst (CompEnd n);
        uncond_jump;
        ldef (CompEnd n)
    ];
    goto_top;
    [
        assign m (neg mreg);
        ldef (CompEnd n)
    ]]

let translate_binop op = List.concat (binop_routines op)
let translate_unop op = List.concat (unop_routines op)
let translate_comp n op = List.concat (comp_routines n op)

let translate_add = translate_binop (dplus mreg)
let translate_sub = translate_binop (minusd mreg)
let translate_and = translate_binop (dand mreg)
let translate_or = translate_binop (dor mreg)

let translate_neg = translate_unop (neg mreg)
let translate_not = translate_unop (bnot mreg)

let translate_eq n = translate_comp n jeq
let translate_lt n = translate_comp n jlt
let translate_gt n = translate_comp n jgt


(* branching operations *)

(* translation for `label ln` inside function `fn` *)
let translate_label ln fn = Llabel (ln, fn)
let translate_goto ln fn = [ainst (Llabel (ln, fn)); uncond_jump]
let translate_ifgoto ln fn =
    let routines = [
        load_top;
        [ainst (Llabel (ln, fn)); djump jne] ] in
    List.concat routines


(* stack manipulation *)

(* base addresses of segments *)
let seg_base = function
    | Local -> 1
    | Argument -> 2
    | This -> 3
    | That -> 4
    | Constant -> -1        (* it's a pseudo-segment, no base address for it *)
    | Static -> 16
    | Pointer -> 3
    | Temp -> 5

let push_routines seg n =
    let base = seg_base seg in
    [
        load_base_offset base n;
        push_d;
    ]

let translate_push_const n = 
    let routines = [ [at n; assign d (iden areg)]; push_d ] in
    List.concat routines
    
let translate_push seg n = 
    match seg with
    | Constant -> translate_push_const n
    | other -> List.concat (push_routines other n)
            
let pop_routines seg n =
    let base = seg_base seg in
    [load_top;
        [
            at (base + n);
            assign m (iden dreg)
        ]
    ]

let translate_pop seg n = List.concat (pop_routines seg n)


(* let translate_push seg offset = *)
(* let translate_pop seg offset = *) 
