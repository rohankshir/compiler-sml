signature REG_ALLOC = 
sig
	structure Frame:FRAME
	type allocation = Frame.register Temp.Table.table
	val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation

end

structure RegAlloc : REG_ALLOC =
struct

	structure Frame = MipsFrame
    type allocation = Frame.register Temp.Table.table

    fun alloc (instrs, frame) =
	let 
	    val (fgraph, nodes) = MakeGraph.instrs2graph instrs
	    val (igraph, liveOut) = Liveness.interferenceGraph fgraph
	    val initial' = Frame.tempMap
	    val registers' = Frame.registers
	    val (allocation, temps) = Color.color {interference = igraph,
						   initial = initial',
						   spillCost = (fn x => 1),
						   registers = registers}
	in
	    (instrs, allocation)
	end

end