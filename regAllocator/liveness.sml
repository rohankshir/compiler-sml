signature LIVENESS = 
sig
	datatype igraph = 
		IGRAPH of {graph: Graph.graph, 
					tnode: Temp.temp -> Graph.node, 
					gtemp: Graph.node -> Temp.temp, 
					moves: (Graph.node * Graph.node) list}

	(*val interferenceGraph : Flow.flowgraph -> 
							igraph * (Flow.Graph.node -> Temp.temp list)

	val show : TextIO.outstream * igraph -> unit*)
end

structure Liveness : LIVENESS = 
struct 

	datatype igraph =
		IGRAPH of {graph: Graph.graph,
					tnode: Temp.temp -> Graph.node,
					gtemp: Graph.node -> Temp.temp,
					moves: (Graph.node * Graph.node) list}

	type liveSet = unit Temp.Table.table * Temp.temp list
	type liveMap = liveSet Flow.Graph.Table.table

	(*fun interferenceGraph (Flow.FGRAPH {control, def, use, ismove}) = 
		let
			val igraph  = IGRAPH {Graph.newGraph() , *)
    	

end