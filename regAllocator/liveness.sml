structure Liveness: 
sig
	datatype igraph = 
		IGRAPH of {graph: IGraph.graph, 
					tnode: Temp.temp -> IGraph.node, 
					gtemp: IGraph.node -> Temp.temp, 
					moves: (IGraph.node * IGraph.node) list}

	val interferenceGraph : Flow.flowgraph -> 
							igraph * (Flow.Graph.node -> Temp.temp list)

	val show : TextIO.outstream * igraph -> unit
end

structure Liveness :> LIVENESS = 
struct 

	type liveSet = unit Temp.Table.table * temp list
	type liveMap = liveSet Flow.Graph.Table.table

	fun interferenceGraph (Flow.FGRAPH {control, def, use, ismove}, nodelist:G.node list) = 
    	let      

end