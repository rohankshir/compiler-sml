structure Main = struct

   structure Tr = Translate
   structure F : FRAME = MipsFrame
   structure S = Symbol
   (*structure R = RegAlloc*)

 fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ (S.name(F.name frame)) ^ "\n")
         val _ = Printtree.printtree(out,body); 
         val stms = Canon.linearize body
         (*val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs =   List.concat(map (Mips.codegen frame) stms') 
         val format0 = Assem.format(F.registerToString)
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

    fun getGraphs filename = 
      let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
           val l = List.mapPartial processFrag frags
           val (flowgraph,nodelist) = hd l
           fun printSucc node = 
            let 
              val nodename = Graph.nodename node
              val successors = map Graph.nodename (Graph.succ node)
              val result = nodename ^ ": " ^ (String.concatWith " " successors) ^ "\n"
            in 
              print result
            end
          val graphString = app printSucc nodelist
        in 
          graphString
       end
    
end


