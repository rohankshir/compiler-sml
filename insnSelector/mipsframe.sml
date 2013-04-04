structure MipsFrame : FRAME =
struct

  datatype access = InFrame of int | InReg of Temp.temp 
  type frame = {name: Temp.label, 
                formals: access list, 
                numLocals: int ref, 
                frameOffset: int ref
                }
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  val wordsize = 4

  val frags = ref [] : frag list ref (* Init frag list to empty list *)
  
  structure T = Tree

  val FP = Temp.newtemp()
  val RV = Temp.newtemp()

  fun string (label,s) = Symbol.name (label) ^ ":   " ^ s

  fun exp (InFrame i) (e) = 
      T.MEM(T.BINOP(T.PLUS, e,T.CONST(i)))
      | exp (InReg r) (t) = T.TEMP(r)

  fun externalCall (s,args) = T.CALL(T.NAME(Temp.namedlabel s), args)

  fun allocFormal (esc, (accs,frameOffset)) =
          (* if formal escapes, add InFrame to access list and push frameOffset down *)
          (case esc of true => (InFrame(frameOffset - wordsize)::accs, frameOffset - wordsize)
          (* otherwise, add InReg to access list and frameOffset stays same *)
                | false => (InReg(Temp.newtemp())::accs, frameOffset))

  fun newFrame{name, formals} = 
     let
            val (access_list,offset) = foldl allocFormal ([],0) formals
     in
            {name=name, formals=access_list, numLocals=ref 0, frameOffset=ref offset}
     end     
  
  fun name(f:frame) = #name f

  fun formals (f:frame) = #formals f
  
  fun allocLocal (f:frame) b = 
    (case b of true =>
            (((#numLocals f):= !(#numLocals f)+ 1);
            ((#frameOffset f) := !(#frameOffset f) - wordsize);
            InFrame(!(#frameOffset f)))
      | false =>
            (((#numLocals f):= !(#numLocals f)+ 1);
            InReg(Temp.newtemp())))

  fun procEntryExit1 (frame, stm) = stm
  fun addFrag f = frags := (f :: !frags)
  fun getResult () = !frags

  fun printFrag (PROC {body, frame})  = (print "\n-----PROC-----\n" ; Printtree.printtree (TextIO.stdOut, body))
    | printFrag (STRING (label,str)) = (print "\n-----STRING-----\n"; print str)

  fun printFragList l = app printFrag l

  fun clearFrags () = (frags := [])
 








end