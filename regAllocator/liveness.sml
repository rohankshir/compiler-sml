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

	structure Set = ListSetFn(struct type ord_key = Temp.temp
								val compare = Int.compare
								end)

	fun interferenceGraph (Flow.FGRAPH {control, def, use, ismove}) = 

		let
			 (*iterate through nodes in reverse*)
			val nodes = rev (Graph.nodes control)

			val liveInT = Graph.Table.empty
			val liveOutT = Graph.Table.empty
			(*initialize livein and live out tables and compute liveness*)
			val (lit,lot) = foldl (fn (n,(lit,lot)) => ((Graph.Table.enter (lit,n,Set.empty)), (Graph.Table.enter (lot,n,Set.empty)))) (liveInT, liveOutT) nodes
			fun repeatCompute(lit,lot,converged) =
				let 
					fun computeLiveness (n, (liveInT, liveOutT, converged)) = 
						let 
							val in_n' = Option.getOpt(Graph.Table.look(liveInT,n),Set.empty)
							val out_n' = Option.getOpt(Graph.Table.look(liveOutT,n),Set.empty)
							val use_n = Set.addList(Set.empty,Option.getOpt(Graph.Table.look(use,n),nil))
							val def_n = Set.addList(Set.empty,Option.getOpt(Graph.Table.look(def,n),nil))
							val succ_n = Graph.succ(n)
							fun union_successors(node, union_set) = Set.union(union_set,getOpt(Graph.Table.look(liveInT,node),Set.empty))
							val out_n= foldl union_successors Set.empty nodes
							val in_n = Set.union(use_n, Set.difference(out_n',def_n))
							val converged' = converged andalso Set.equal(in_n',in_n) andalso Set.equal(out_n',out_n)
							val liveInT' = Graph.Table.enter(liveInT,n,in_n)
							val liveOutT' = Graph.Table.enter(liveOutT,n,out_n)
						in
						(liveInT',liveOutT',converged')
						end
					val (lit,lot,converged') = foldl computeLiveness (liveInT,liveOutT,converged) nodes
				in 
					if (converged = false) then repeatCompute(lit,lot,true) else (lit,lot)
				end
			val (liveInT',liveOutT') = repeatCompute(liveInT,liveOutT,true)

			(*initialize graph and relevant node temp tables*)
			fun getTemps (n, tempSet) = 
				let val use_n = Set.addList(Set.empty,Option.getOpt(Graph.Table.look(use,n),nil))
					val def_n = Set.addList(Set.empty,Option.getOpt(Graph.Table.look(def,n),nil))
					val def_and_use = Set.union(use_n,def_n)
				in 
					Set.union(tempSet,def_and_use)
				end
			val allTemps = Set.listItems(foldl getTemps Set.empty nodes)
			val iGraph = Graph.newGraph()

			fun initializeGraph(t,(node_map,temp_map)) = 
				let val node = Graph.newNode(iGraph)
					val node_map' = Graph.Table.enter(node_map,node,t)
					val temp_map' = Temp.Table.enter(temp_map,t,node)
				in
				(node_map',temp_map')
				end

			val (node_map,temp_map) = foldl initializeGraph (Graph.Table.empty,Temp.Table.empty) allTemps
		in 
		()
		end



		

	
    	

end