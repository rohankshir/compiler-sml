signature MAKEGRAPH=
sig
	val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
end

structure Makegraph :> MAKEGRAPH = 
struct

structure A = Assem

fun instrs2graph instrs = 
	let
		val nodeList = nil
		val graph = Flow.FGRAPH{control = Graph.newGraph(), 
							 def = Graph.Table.empty,
							 use = Graph.Table.empty,
							 ismove = Graph.Table.empty}

		fun buildNodes (instr, (Flow.FGRAPH{control,def,use,ismove}, nodes)) = 
			let 
				fun getTableEntries (A.OPER{assem, dst, src, jump}) = (dst,src,false)
					| getTableEntries (A.MOVE{assem, dst, src}) = ([dst],[src],true)
					| getTableEntries (A.LABEL{assem, lab}) = (nil,nil,false)

				val (defEntry, useEntry, ismoveEntry) = getTableEntries instr
				val node = Graph.newNode control
			in 
				(Flow.FGRAPH{control = control,
                       def = Graph.Table.enter(def,node,defEntry),
                       use = Graph.Table.enter(use,node, useEntry),
                       ismove = Graph.Table.enter(ismove,node,ismoveEntry)},
             	rev(node::rev(nodes)))
            end

		val (graph, nodeList) = foldl buildNodes (graph, nodeList) instrs

		fun getLabels (A.LABEL {assem,lab}, node) = SOME (lab,node)
			| getLabels _ = NONE

		val labelNodePairs = List.mapPartial getLabels (ListPair.zip (instrs, nodeList))

		fun getLabelsNode label = 
			let 
				val pair = List.find (fn(lab,node) => (label = lab)) labelNodePairs
			in 
				case pair of SOME (l,node) => SOME node | NONE => NONE
			end

        fun connectNodes ((Assem.OPER {assem, dst, src, jump=SOME labels}, node), _ ) = 
        	(let 
        		fun makeJumpEdge l = (case (getLabelsNode l) of SOME n => Graph.mk_edge {from = node, to = n} | _ => ())
        	in 
        		(app makeJumpEdge labels; SOME node)
        	end)
        	| connectNodes ((instr, node), SOME nextNode) = (Graph.mk_edge {from = node, to = nextNode}; SOME node)
        	| connectNodes ((instr, node), NONE) = SOME node
                 
		val _ = foldr connectNodes NONE (ListPair.zip(instrs, nodeList))

      in
        (graph,nodeList)
      end


end