signature CODEGEN = 
sig
	structure FRAME : FRAME
	val codegen : FRAME.frame -> Tree.stm -> Assem.instr list
end