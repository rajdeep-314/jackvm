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
        ainst (CompSucc n);
        djump op
    ];
    goto_top;
    [
        assign m (zero);
        ainst (CompEnd n);
        uncond_jump;
        ldef (CompSucc n)
    ];
    goto_top;
    [
        assign m (minusone);
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

(* n denotes the "index" of the comparison instruction *)
let translate_eq n = translate_comp n jeq
let translate_lt n = translate_comp n jlt
let translate_gt n = translate_comp n jgt


(* branching operations *)

(* translation for `label ln` inside function `fn` *)
let translate_label ln fn = [ldef (Llabel (ln, fn))]
let translate_goto ln fn = [ainst (Llabel (ln, fn)); uncond_jump]
let translate_ifgoto ln fn =
    let routines = [
        load_top;
        [ainst (Llabel (ln, fn)); djump jne] ] in
    List.concat routines


(* stack manipulation *)

(* base addresses of segments *)
let seg_base = function
    | This -> 3
    | That -> 4
    | Static -> 16
    | Pointer -> 3
    | Temp -> 5
    | _ -> -1               (* different translation mechanisms needed *)

let seg_pointer = function
    | Local -> 1
    | Argument -> 2
    | _ -> -1               (* different translation mechanisms needed *)

let push_routines_base seg n =
    let base = seg_base seg in
    [
        load_base_offset base n;
        push_d;
    ]

let push_routines_pointer seg n =
    let pointer = seg_pointer seg in
    [
        load_pointer_offset pointer n;
        push_d
    ]

let translate_push_const n = 
    let routines = [ [at n; assign d (iden areg)]; push_d ] in
    List.concat routines
    
let translate_push seg n = 
    match seg with
    | Constant -> translate_push_const n
    | Local -> List.concat (push_routines_pointer Local n)
    | Argument -> List.concat (push_routines_pointer Argument n)
    | other -> List.concat (push_routines_base other n)
            
let pop_routines_base seg n =
    let base = seg_base seg in
    [
        load_top;
        [
            at (base + n);
            assign m (iden dreg)
        ]
    ]

let pop_routines_pointer seg n =
    let pointer = seg_pointer seg in
    [
        load_pointer_offset pointer n;
        [
            assign d (iden areg);
            at 5;                       (* temp[0] is the address 5 *)
            assign m (iden dreg);
        ];
        load_top;
        [
            at 5;
            assign a (iden mreg);
            assign m (iden dreg);
        ]
    ]

let translate_pop seg n =
    match seg with
    | Local -> List.concat (pop_routines_pointer Local n)
    | Argument -> List.concat (pop_routines_pointer Argument n)
    | other -> List.concat (pop_routines_base other n)


(* functions *)

(* begin here *)


(* for testing *)

let translate_inst inst n =
    match inst with
    | Add -> translate_add
    | Sub -> translate_sub
    | Neg -> translate_neg
    | And -> translate_and
    | Or -> translate_or
    | Not -> translate_not
    | Eq -> translate_eq n
    | Lt -> translate_lt n
    | Gt -> translate_gt n
    | Push (seg, n) -> translate_push seg n
    | Pop (seg, n) -> translate_pop seg n
    (* only for temp testing *)
    | Label name -> translate_label name (Fname "")
    | Goto name -> translate_goto name (Fname "")
    | IfGoto name -> translate_ifgoto name (Fname "")
    (* /only for temp testing *)
    | _ -> []

let rec nat_nums n =
    if n = 0 then []
    else nat_nums (n-1) @ [n]

(* and infinite loop to be put at the end of
   the translated ASM program *)
let prog_end = [
        ldef (Symb "end_of_prog");
        ainst (Symb "end_of_prog");
        uncond_jump
    ]

let translate_prog prog =
    let lns = nat_nums (List.length prog) in
    let routines = List.map2 translate_inst prog lns in
    List.concat routines @ prog_end
