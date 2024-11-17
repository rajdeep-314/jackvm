(*
    bootstrap.ml

    Bootstrap code - code that will set up the 'environment'
    for the translated VM code to be executed, namely:
        - Initializing SP to 256
        - Calling Sys.init
*)

open Jackvm.Ast
open Jackvm.Ast.Helper
open Asm
open Assembler.Ast.Helper

(* assembler code to initialize SP to 256 *)
let sp_init_asm = [
    at 256;
    assign d (iden areg);
    ainst (Symb "SP");
    assign m (iden dreg); ]

(* a call to Sys.init, with 0 arguments *)
let sys_init_call = call (Fname "Sys.init") 0

(* the Sys.init function, where the execution begins -
   it calls Main.main and then enters an infinite loop *)
let sys_init_func =
    { name = Fname "Sys.init";
      locals = 0;
      body = [
          call (Fname "Main.main") 0;
          label "INF_LOOP";
          goto "INF_LOOP" ] }

(* ASM translations of the above VM snippets *)
let sys_init_call_asm = Translate.translate_call (Fname "Sys.init") 0 (Fname "") 0
let sys_init_func_asm = Translate.translate_function ".vm" sys_init_func

(* the complete bootstrapping ASM code *)
let bootstrap_asm = sp_init_asm @ sys_init_call_asm @ sys_init_func_asm

