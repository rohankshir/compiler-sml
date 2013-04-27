structure Color :> COLOR = 
struct
	structure Frame = MipsFrame
	type allocation = Frame.register Temp.Table.table

	structure Stack: 
	sig
		val push: 'a list * 'a  -> 'a list
		val pop: 'a list -> ('a * 'a list) option
		val empty: 'a list -> bool
	end = 
	struct 
		fun push (l, a) = a::l

		fun pop [] = NONE
		| 	pop (a::l) = SOME(a,l)

		fun empty [] = true
		| 	empty _ = false
	end

	fun color {interference = Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} = 

		let 
			val nodes = Graph.nodes graph
			val allTemps = map gtemp nodes
			val K = length(Frame.registers)
			fun nodesAreAdj (node1, node2) = 
				let 
					val adjList = Graph.adj node1
					fun eqNode2 n = Graph.eq (node2, n)
				in 
					List.exists eqNode2 adjList
				end

			fun buildAdjTable (node,adjTable) = Graph.Table.enter(adjTable,node,Graph.adj node)

			fun degree node = length(Graph.adj node)

			fun addUncolored (t,uncolored) = case Temp.Table.look(initial,t) of
						SOME(register) => uncolored
					|   NONE => t::uncolored
			fun makeWorklist (n,(spillWorklist, simplifyWorklist)) = 
				if (degree n) >= K
				then (n::spillWorklist,simplifyWorklist)
				else (spillWorklist,n::simplifyWorklist)

			val uncoloredTemps = foldr addUncolored [] allTemps
			val uncoloredNodes = map tnode uncoloredTemps
			val adjTable = foldl buildAdjTable Graph.Table.empty uncoloredNodes
			val (spillWorklist,simplifyWorklist) = foldr makeWorklist (nil,nil) uncoloredNodes


			fun simplify(n,(selectStack)) = 
				let
					val selectStack' = Stack.push(selectStack,n)
					fun removeEdges node = 
						let
							fun removeEdge v = Graph.rm_edge {from=node,to=v}
						in
							app removeEdge (Graph.adj node)
						end
					val () = removeEdges n
				in
					selectStack'
				end
			val selectStack = foldl simplify nil simplifyWorklist
		in 
			(initial, [])
		end
end