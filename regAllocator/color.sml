structure Color : COLOR = 
struct
	structure Frame = MipsFrame
	type allocation = Frame.register Temp.Table.table

	(*structure Stack: 
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
	end*)

	structure Set = ListSetFn(struct type ord_key = Temp.temp
								val compare = Int.compare
								end)

	structure StringSet = ListSetFn(struct type ord_key = string
								val compare = String.compare
								end)

	fun remove_edge(node1,node2) = 
		if (List.exists (fn n =>Graph.eq(node1,n)) (Graph.succ node2))
		then Graph.rm_edge {from=node2, to = node1}
		else Graph.rm_edge {from=node1, to = node2}
	fun color {interference = interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} = 

		let 
			
			val nodes = Graph.nodes graph
			val allTemps = map gtemp nodes
			val K = length(Frame.registers)
			(* Currently all the colors are just string representation of numbers from 1 to K*)
			val colorSet = StringSet.addList(StringSet.empty,registers)
			fun nodesAreAdj (node1, node2) = 
				let 
					val adjList = Graph.adj node1
					fun eqNode2 n = Graph.eq (node2, n)
				in 
					List.exists eqNode2 adjList
				end
			(*save all the adjacencies in a table for when we modify *)
			fun buildAdjTable (node,adjTable) = Graph.Table.enter(adjTable,node,Graph.adj node)

			fun degree node = length(Graph.adj node)

			fun addUncolored (t,uncolored) = case Temp.Table.look(initial,t) of
						SOME(register) => uncolored
					|   NONE => t::uncolored
			fun makeWorklist (n,(spillWorklist, simplifyWorklist)) = 
				if (degree n) >= K
				then (n::spillWorklist,simplifyWorklist)
				else (spillWorklist,n::simplifyWorklist)

			fun editWorklist (n,simplifyWorklist) = 
				if (degree n) < K andalso (degree n) > 0
				then (n::simplifyWorklist)
				else (simplifyWorklist)

			val uncoloredTemps = foldr addUncolored [] allTemps
			val uncoloredNodes = map tnode uncoloredTemps
			val precoloredTempSet = Set.addList(Set.empty,Frame.machineTemps)
			val uncoloredTempsSet = Set.addList(Set.empty,uncoloredTemps)
			val adjTable = foldl buildAdjTable Graph.Table.empty nodes
			fun getAdjList n = case Graph.Table.look(adjTable,n) of
									SOME(l) => l
								|	NONE => ((ErrorMsg.error 0 "wtf yo");nil)
			val (spillWorklist,simplifyWorklist) = foldr makeWorklist (nil,nil) uncoloredNodes

			fun filterUncolored node = Set.member(uncoloredTempsSet,gtemp node)

			val () = print "Initialized worklists...\n"
			fun simplify(n,(selectStack, simplifyWorklist)) = 
				let
					val selectStack' = n::selectStack
					fun removeEdges node = 
						let
							(*val () = Liveness.show(TextIO.stdOut,interference)*)
							fun removeEdge v = remove_edge (node,v)
						in
							app removeEdge (Graph.adj node)
						end
					val () = removeEdges n
					val simplifyWorklist' = foldl editWorklist simplifyWorklist (List.filter filterUncolored (Graph.adj n))
				in
					(selectStack',simplifyWorklist')
				end

			fun repeatSimplify (selectStack, simplifyWorklist) = 
				let
					val (selectStack',simplifyWorklist') = foldl simplify (selectStack,nil) simplifyWorklist
				in
					case simplifyWorklist' of 
						[] => selectStack'
					|	_  => repeatSimplify(selectStack', simplifyWorklist')
				end
			val selectStack = repeatSimplify(nil,simplifyWorklist)
			val () = print "Simplified...\n"

			fun checkSpilling n = 
				if (degree n) >= K 
				then ErrorMsg.impossible "Spilling not implemented"
				else ()
			val () = app checkSpilling uncoloredNodes 
			fun assignColors (n,(coloredTempSet, colorTable)) =
				let
					fun colorOf n = case Temp.Table.look(colorTable,gtemp n) of
						SOME(color) => color
					|	NONE => (ErrorMsg.error 0 "node is not in color table"; "9999")

					fun getRemainingColors (w,colorsLeft) = 
						if Set.member(coloredTempSet,(gtemp w)) orelse Set.member(precoloredTempSet,(gtemp w))
						then (if StringSet.member(colorsLeft,colorOf w) 
								then StringSet.delete(colorsLeft,colorOf w) 
								else colorsLeft)
						else colorsLeft
					val okColors = foldl getRemainingColors colorSet (getAdjList n)
(*					val () = print "Got remaining colors...\n"*)
					val () = if StringSet.isEmpty(okColors)
							 then ErrorMsg.impossible "spilling not implemented"
							 else ()
					val coloredTempSet' = Set.add(coloredTempSet,gtemp n)
					(*allocate first color of okColors to n*)
					val color_n = hd (StringSet.listItems(okColors))
					val colorTable' = Temp.Table.enter(colorTable,gtemp n,color_n)

				in
					(coloredTempSet',colorTable')
				end
			val () = print "Colored...\n"
			val (coloredTempSet,resultAllocation) = foldl assignColors (Set.empty,initial) selectStack

		in 
			(resultAllocation, [])
		end
end