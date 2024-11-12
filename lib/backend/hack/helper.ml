include Jackvm.Ast.JVMHelper
include Asm
let label x = Jackvm.Ast.Label (Lname x)
let goto x = Jackvm.Ast.Goto (Lname x)
let ifgoto x = Jackvm.Ast.IfGoto (Lname x)
