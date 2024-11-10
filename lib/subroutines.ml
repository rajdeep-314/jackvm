(*
    subroutines.ml

    subroutines here refer to a bunch of ASM instructions that are
    frequently used in the translated VM instructions
    they make the code easier to understand and maintain
*)

open Asm
open Assembler.Ast.Helper

(* loads SP-1 into A, doesn't alter SP *)
let goto_top = [
    ainst (Symb "SP");
    assign a (pred mreg) ]

(* loads RAM[*SP] into D, decrements SP by 1 *)
let load_top = [
    ainst (Symb "SP");
    assign am (pred mreg);
    assign d (iden mreg) ]

(* loads `base + off` into A *)
let goto_base_offset base off = [
    at (base + off) ]

(* loads `RAM[base + off]` into D *)
let load_base_offset base off = 
    goto_base_offset base off @ [assign d (iden mreg)]

(* loads `*pointer + off` into A *)
let goto_pointer_offset pointer off = [
    at off;
    assign d (iden areg);
    at pointer;
    assign a (dplus mreg) ]

(* loads `RAM[*pointer + off]` into D *)
let load_pointer_offset pointer off =
    goto_pointer_offset pointer off @ [assign d (iden mreg)]

(* increments SP by 1 *)
let incr_sp = [
    ainst (Symb "SP");
    assign m (succ mreg) ]

let push_d = [
    ainst (Symb "SP");
    assign a (iden mreg);
    assign m (iden dreg);
    ainst (Symb "SP") ] @ incr_sp


(* function implementation related subroutines *)

let push_pointer name = [
    ainst (Symb name);
    assign d (iden mreg) ] @ push_d

(* used when RAM[SP-1] has a pointer-based segment's old value,
   updates the segment's pointer register with said value, while
   decrementing SP by 1  *)
let restore_pointer name = load_top @ [
    ainst (Symb name);
    assign m (iden dreg);
    ]

(* D = RAM[n] *)
let get_from_address n = [
    at n;
    assign d (iden mreg)
]

(* RAM[n] = D *)
let put_to_address n = [
    at n;
    assign m (iden dreg)
]

(* D = RAM[@(Symb name)] *)
let get_from_symb name = [
    ainst (Symb name);
    assign d (iden mreg);
]

(* RAM[@(Symb name)] = D *)
let put_to_symb name = [
    ainst (Symb name);
    assign m (iden dreg);
]
