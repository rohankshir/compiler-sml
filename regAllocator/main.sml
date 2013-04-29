structure Main = struct

   structure Tr = Translate
   structure F : FRAME = MipsFrame
   structure S = Symbol
   structure R = RegAlloc

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ (S.name(F.name frame)) ^ "\n")
         (*val _ = Printtree.printtree(out,body); *)
         val stms = Canon.linearize body
         (*val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs =   F.procEntryExit2(frame,List.concat(map (Mips.codegen frame) stms'))
        val {prolog, body, epilog} = F.procEntryExit3(frame,instrs)
        val (_,allocation) = R.alloc(instrs,frame)
        fun regToString temp = 
          case Temp.Table.look(allocation,temp) of 
            SOME(reg) => reg
          | NONE => Temp.makestring(temp)
         val format0 = Assem.format(regToString)
      in  app (fn i => TextIO.output(out,format0 i)) instrs
     end
    | emitproc out (F.STRING (lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
      handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
       (fn out => (map (emitproc out) frags))
       end

    fun processFrag (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ (S.name(F.name frame)) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
         val stms = Canon.linearize body
         (*val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val instrs =   List.concat(map (Mips.codegen frame) stms')
      in
          SOME(MakeGraph.instrs2graph instrs)
      end
    | processFrag (F.STRING (lab,s)) = NONE


    fun printGraph nodeList = 
      let 
        fun printSucc node = 
            let 
              val nodename = Graph.nodename node
              val successors = map Graph.nodename (Graph.succ node)
              val result = nodename ^ ": " ^ (String.concatWith " " successors) ^ "\n"
            in 
              print result
            end
          in
            app printSucc nodeList
          end


    fun getGraphs filename = 
      let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
           val l = List.mapPartial processFrag frags     
        in 
          app printGraph (#2 (ListPair.unzip(l)))
       end

      fun printLiveness filename = 
        let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
           val l = List.mapPartial processFrag frags
           fun printHelper (flowgraph,nodelist) = 
            let
              val () = (print "\nFlowgraph:\n";printGraph nodelist)
              val igraph = Liveness.interferenceGraph flowgraph
              val () = (print "\nIGraph:\n"; Liveness.show (TextIO.stdOut,#1 igraph))
            in
            ()
            end

        in
          app printHelper l 
        end
    
end



