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

(* loads (base + off) into A *)
let goto_base_offset base off = [
    at off;
    assign d (iden areg);
    at base;
    assign a (dplus mreg) ]

(* loads RAM[base + off] into D *)
let load_base_offset base off = 
    goto_base_offset base off @ [assign d (iden mreg)]

(* increments SP by 1 *)
let incr_sp = [
    ainst (Symb "SP");
    assign m (succ mreg) ]

let push_d = [
    ainst (Symb "SP");
    assign a (iden mreg);
    assign m (iden dreg);
    ainst (Symb "SP") ] @ incr_sp;
