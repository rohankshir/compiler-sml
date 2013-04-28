structure MipsFrame : FRAME =
struct

  datatype access = InFrame of int | InReg of Temp.temp 
  type frame = {name: Temp.label, 
                formals: access list, 
                numLocals: int ref, 
                frameOffset: int ref
                }
  type register = string


  datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
  val wordsize = 4
  val numArgs = 4

  val frags = ref [] : frag list ref (* Init frag list to empty list *)
  
  structure T = Tree
  structure A = Assem

  val FP = Temp.newtemp()
  val RV = Temp.newtemp()
  val RA = Temp.newtemp()
  val ZERO = Temp.newtemp()
  val SP = Temp.newtemp()
  val GP = Temp.newtemp()
  (*double check this*)
  
  val specialregs = [FP,RV,RA,SP,ZERO, GP]

  fun regBuilder i = Temp.newtemp()
  val argregs = List.tabulate(4,regBuilder)
  val callersaves = List.tabulate(10,regBuilder)
  val calleesaves = List.tabulate(8,regBuilder)
  val randomRegs = List.tabulate(2,regBuilder)
  val calldefs = [FP,RV,RA] @ callersaves
  val machineTemps = specialregs @ argregs @ callersaves @ calleesaves @ randomRegs
  val registers = ["$fp","$v0","$ra", "$zero","$gp", "$a0", "$a1", "$a2", "$a3","$t0" ,"$t1" ,"$t2" ,"$t3" ,"$t4" ,"$t5" ,"$t6" ,"$t7" ,"$t8" , "$t9", "$s0","$s1","$s2", "$s3", "$s4", "$s5", "$s6", "$s7" , "$at", "$v1"]

  fun buildTempMap() = 
    let
      fun addToMap ((temp,register), table ) = Temp.Table.enter(table,temp,register)
    in 
      foldl addToMap  Temp.Table.empty (ListPair.zip(machineTemps,registers))
    end

  val tempMap = buildTempMap()

  fun registerToString temp = 
    case Temp.Table.look(tempMap,temp) of
      SOME(register) => register
    | NONE => Temp.makestring(temp)


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

  
  fun seq [] = T.EXP (T.CONST 0)
      | seq [s] = s
      | seq (h::l) = T.SEQ(h,seq l)

  fun viewShift (f:frame) = 
    let
      fun helper (access, arg) = T.MOVE (exp access (T.TEMP FP),T.TEMP arg)
      val access_list = formals f
    in 
      seq (map helper (ListPair.zip(access_list, argregs)))
    end


  fun procEntryExit1 (frame, stm) =
    let
      val inMem = [RA] @ calleesaves
      val inRegs = map regBuilder inMem
      fun moveTemps (dst,src) = T.MOVE (T.TEMP dst, T.TEMP src)
      val saveTree = map moveTemps (ListPair.zip(inRegs, inMem))
      val restoreTree = map moveTemps (ListPair.zip(inMem, inRegs))
    in 
      T.SEQ (viewShift(frame), seq (saveTree @ [stm] @ restoreTree))
    end



  fun procEntryExit2 (frame, body) = 
    body @ 
      [A.OPER{assem="",
              src=specialregs @ calleesaves,
              dst=[],jump=SOME[]}]
           
  fun procEntryExit3 (frame, body) =
      {prolog = "PROCEDURE " ^ Symbol.name(name(frame)) ^ "\n",
       body = body,
       epilog = "END " ^ Symbol.name (name(frame)) ^ "\n"}

  fun addFrag f = frags := (f :: !frags)
  fun getResult () = !frags

  fun printFrag (PROC {body, frame})  = (print ("\n-----PROC: " ^ Symbol.name (name (frame)) ^"-----\n") ; Printtree.printtree (TextIO.stdOut, body))
    | printFrag (STRING (label,str)) = (print "\n-----STRING-----\n"; print str)

  fun printFragList l = app printFrag l

  fun clearFrags () = (frags := [])
 








end