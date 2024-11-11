(*
    translate.ml
    
    contains functions to translate VM code to it's corresponding
    ASM code
*)


open Ast
open Asm
open Subroutines
open Bootstrap
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
let comp_routines fn n op = [
    load_top;
    goto_top;
    [
        assign d (minusd mreg);
        ainst (CompSucc (fn, n));
        djump op
    ];
    goto_top;
    [
        assign m (zero);
        ainst (CompEnd (fn, n));
        uncond_jump;
        ldef (CompSucc (fn, n))
    ];
    goto_top;
    [
        assign m (minusone);
        ldef (CompEnd (fn, n))
    ]]

let translate_binop op = List.concat (binop_routines op)
let translate_unop op = List.concat (unop_routines op)
let translate_comp fn n op = List.concat (comp_routines fn n op)

let translate_add = translate_binop (dplus mreg)
let translate_sub = translate_binop (minusd mreg)
let translate_and = translate_binop (dand mreg)
let translate_or = translate_binop (dor mreg)

let translate_neg = translate_unop (neg mreg)
let translate_not = translate_unop (bnot mreg)

(* n denotes the "index" of the comparison instruction *)
let translate_eq fn n = translate_comp fn n jeq
let translate_lt fn n = translate_comp fn n jlt
let translate_gt fn n = translate_comp fn n jgt


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
    | Static -> 16
    | Pointer -> 3
    | Temp -> 5
    | _ -> -1               (* different translation mechanisms needed *)

let seg_pointer = function
    | Local -> 1
    | Argument -> 2
    | This -> 3
    | That -> 4
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
    | Static -> List.concat (push_routines_base Static n)
    | Pointer -> List.concat (push_routines_base Pointer n)
    | Temp -> List.concat (push_routines_base Temp n)
    | other -> List.concat (push_routines_pointer other n)

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
    | Static -> List.concat (pop_routines_base Static n)
    | Pointer -> List.concat (pop_routines_base Pointer n)
    | Temp -> List.concat (pop_routines_base Temp n)
    | other -> List.concat (pop_routines_pointer other n)


(* functions *)

let call_routines callee_name nargs caller_name ln = [
    [
        at nargs;
        assign d (iden areg);
    ];
    put_to_address 5;
    get_from_symb "SP";
    [
        at 5;
        assign m (dminus mreg);
        ainst (Fret (callee_name, caller_name, ln));
        assign d (iden areg);
    ];
    push_d;
    push_pointer "LCL";
    push_pointer "ARG";
    push_pointer "THIS";
    push_pointer "THAT";

    get_from_address 5;
    put_to_symb "ARG";
    get_from_symb "SP";
    put_to_symb "LCL";

    [
        ainst (Fcall callee_name);
        uncond_jump;                                        (* transferring control to the function *)
        ldef (Fret (callee_name, caller_name, ln));         (* defining the label where fname should return to *)
    ] ]

let translate_call callee_name nargs caller_name ln = List.concat (call_routines callee_name nargs caller_name ln)

let return_routines = [
    load_top;
    put_to_address 5;
    get_from_symb "ARG";
    put_to_address 6;
    get_from_symb "LCL";
    put_to_symb "SP";

    restore_pointer "THAT";
    restore_pointer "THIS";
    restore_pointer "ARG";
    restore_pointer "LCL";
    
    load_top;
    put_to_address 7;
    get_from_address 6;
    put_to_symb "SP";
    get_from_address 5;
    push_d;
    [
        at 7;
        assign a (iden mreg);
        uncond_jump
    ] ]

let translate_return = List.concat return_routines

(* makes a list with `n` elements, all being `ele` *)
let rec list_make ele n =
    if n = 0 then []
    else ele :: (list_make ele (n-1))

let funcdef_routines fname nlocals =
    [ [ldef (Fcall fname); assign d zero] ] @ (list_make push_d nlocals)

(* a function's 'preamble' *)
let translate_funcdef fname nlocals = List.concat (funcdef_routines fname nlocals)


(* fname : function name
   ln    : line number  *)
let translate_inst fname ln = function
    | Add -> translate_add
    | Sub -> translate_sub
    | Neg -> translate_neg
    | And -> translate_and
    | Or -> translate_or
    | Not -> translate_not
    | Eq -> translate_eq fname ln
    | Lt -> translate_lt fname ln
    | Gt -> translate_gt fname ln
    | Push (seg, n) -> translate_push seg n
    | Pop (seg, n) -> translate_pop seg n
    | Call (fn, n) -> translate_call fn n fname ln
    | Return -> translate_return
    | Label name -> translate_label name fname
    | Goto name -> translate_goto name fname
    | IfGoto name -> translate_ifgoto name fname

(* a helper function to generate the first `n` positive natural numbers *)
let rec nat_nums n =
    if n = 0 then []
    else nat_nums (n-1) @ [n]

let translate_body fname body =
    let line_nums = nat_nums (List.length body) in
    let translations = List.map2 (translate_inst fname) line_nums body in
    List.concat translations
    
let translate_function { name = fname; locals = nlocs; body = body } =
    let preamble = translate_funcdef fname nlocs in
    let body_trans = translate_body fname body in
    preamble @ body_trans


(* the combined translation function, which incorporates
   the bootstrap code from bootstrap.ml as well *)
let translate_prog (prog : ('f, 'l) program) =
    let complete_prog = sys_init_func :: prog in
    let trans_routines = List.map translate_function complete_prog in
    let complete_trans = asm_bootstrap @ (List.concat trans_routines) in
    complete_trans

