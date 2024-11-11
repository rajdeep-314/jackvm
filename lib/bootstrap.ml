(*
    bootstrap.ml

    bootstrap code - code that will set up the 'environment'
    for the translated VM code to be executed
*)

open Ast
open Ast.Helper
open Assembler.Ast.Helper

(* some assembler bootstrap code, it sets SP
   to be 256 *)
let asm_bootstrap = [
    at 256;
    assign d (iden areg);
    ainst (Symb "SP");
    assign m (iden dreg); ]

(* the Sys.init function, where the execution begins
   it calls Main.main and then gets in an infinite loop *)
let sys_init_func =
    { name = Fname "Sys.init";
      locals = 0;
      body = [
          call (Fname "Main.main") 0;
          label "INF_LOOP";
          goto "INF_LOOP" ] }
