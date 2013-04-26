signature LIVENESS = 
sig
	datatype igraph = 
		IGRAPH of {graph: Graph.graph, 
					tnode: Temp.temp -> Graph.node, 
					gtemp: Graph.node -> Temp.temp, 
					moves: (Graph.node * Graph.node) list}

	val interferenceGraph : Flow.flowgraph -> 
							igraph * (Flow.Graph.node -> Temp.temp list)

	val show : TextIO.outstream * igraph -> unit
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

	fun show (outstream, IGRAPH{graph,tnode,gtemp,moves}) = 
		let 
			val nodeToString = Temp.makestring o gtemp
			fun printNode node = 
				let 
					val adjListString = map nodeToString (Graph.adj node)
				in
					TextIO.output (outstream,((nodeToString node) ^ "---> " ^
										(String.concatWith "," adjListString) ^ "\n"
										))
				end
		in
			app printNode (Graph.nodes graph)
		end

	fun printSet set = 
		let
			val stringList= map Temp.makestring (Set.listItems set)
			val result = String.concatWith ", " stringList
		in 
		print (result ^ "\n")
		end

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
							val out_n= foldl union_successors Set.empty succ_n
							val in_n = Set.union(use_n, Set.difference(out_n',def_n))
							val converged' = converged andalso (Set.equal(in_n',in_n)) andalso (Set.equal(out_n',out_n))
							(*val () = (print ("Computing Liveness:" ^ (Graph.nodename n) ^ ": \nOut set:\n"); printSet out_n;print "In Set:\n";printSet in_n; print ("Converged: " ^ Bool.toString(converged) ^ "\n\n"))*)
							val liveInT' = Graph.Table.enter(liveInT,n,in_n)
							val liveOutT' = Graph.Table.enter(liveOutT,n,out_n)
						in
						(liveInT',liveOutT',converged')
						end
					val (lit,lot,converged') = foldl computeLiveness (lit,lot,converged) nodes
				in 
					if (converged' = false) then repeatCompute(lit,lot,true) else (lit,lot)
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
			val () = (print "All temps: \n"; printSet (Set.addList(Set.empty,allTemps)))
			val graph = Graph.newGraph()

			fun initializeGraph(t,(node_map,temp_map)) = 
				let val node = Graph.newNode(graph)
					val node_map' = Graph.Table.enter(node_map,node,t)
					val temp_map' = Temp.Table.enter(temp_map,t,node)
					val exists = case Graph.Table.look(node_map',node) of 
						SOME(t) => "exists" 
					|	NONE => "does not exist"
					val () = print ((Graph.nodename node) ^ ": " ^(Temp.makestring t) ^ " =>" ^ exists ^ "\n")
				in
				(node_map',temp_map')
				end

			val (node_map,temp_map) = foldl initializeGraph (Graph.Table.empty,Temp.Table.empty) allTemps
			

			fun tnode temp = case Temp.Table.look(temp_map, temp) of
				SOME(node) => node
			| 	NONE => (ErrorMsg.error 1 ("no such temp "^ (Temp.makestring temp) ^" in temp map");hd nodes)

			fun gtemp node = case Graph.Table.look(node_map, node) of
				SOME(temp) => temp
			| 	NONE => (ErrorMsg.error 1 ("no such node "^ (Graph.nodename node) ^" in node map");Temp.newtemp())

			val moves = ref[]

			fun buildNodesEdges node = 
				let
					val defList = getOpt(Graph.Table.look(def, node), nil)
					val useSet = Set.addList(Set.empty, getOpt(Graph.Table.look(use,node), nil))
					val liveOutSet = getOpt(Graph.Table.look(liveOutT', node), Set.empty)
					val isMove = getOpt(Graph.Table.look(ismove, node), false)
					val () = (print ("Node name: " ^ (Graph.nodename node) ^ ": \nUse Set:\n"); printSet useSet; print "live out set:\n";printSet liveOutSet)

					fun nodesAreAdj (node1, node2) = 
						let 
							val adjList = Graph.adj node1
							fun eqNode2 n = Graph.eq (node2, n)
						in 
							List.exists eqNode2 adjList
						end

					fun edgeMaker temp1 = 
						let
							fun mkEdge temp2 = 
								let
									val node1 = tnode temp1
									val node2 = tnode temp2
									val () = print ("\nTemp 1: " ^ (Temp.makestring temp1) ^ " Node 1: " ^ (Graph.nodename node1) ^ 
										"\n")
									val () = print ("\nTemp 2: " ^ (Temp.makestring temp2) ^ " Node 2: " ^ (Graph.nodename node2) ^ 
										"\n")

									fun mkMoveList (temp1, temp2) = 
										if (isMove andalso Set.member(useSet, temp2))
										then moves := ((node1,node2)::(!moves))
										else ()

								in 
									if (nodesAreAdj(node1, node2) orelse temp1=temp2)
									then ()
									else 
										if isMove andalso Set.member(useSet, temp2)
										then mkMoveList(temp1, temp2)
										else Graph.mk_edge{from = node1, to = node2}
								end
							in 
								app mkEdge (Set.listItems(liveOutSet))
							end
				in
					app edgeMaker defList
				end
			val () = app buildNodesEdges nodes
			fun nodeToTemps n = Set.listItems(valOf(Graph.Table.look(liveOutT', n)))


		in 
			(IGRAPH {graph=graph, tnode=tnode, gtemp=gtemp, moves=(!moves)},
	 			nodeToTemps)
		end



		

	
    	

end