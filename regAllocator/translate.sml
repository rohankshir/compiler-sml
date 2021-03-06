signature TRANSLATE = 
sig 
	type level 
	type access (* not the same as Fram.access *)
	type exp 
	type breakpoint
	type frag

  val ERROR : exp

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access

	val unEx : exp -> Tree.exp
 	val unNx : exp -> Tree.stm
 	val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

  val intLiteral : int -> exp
  val stringLiteral : string -> exp

  (* dem binops,  dho *)
  val add: exp * exp -> exp
  val minus: exp * exp -> exp
  val mult: exp * exp -> exp
  val divide: exp * exp -> exp
  val eq:  exp * exp -> exp
  val neq:  exp * exp -> exp
  val lt:  exp * exp -> exp
  val gt:  exp * exp -> exp
  val le:  exp * exp -> exp
  val ge:  exp * exp -> exp

  val stringEq:  exp * exp -> exp
  val stringNeq:  exp * exp -> exp




 	val nilExp : unit -> exp
  val recordExp: exp list -> exp
  val arrayExp: exp * exp -> exp
  val ifExp: exp * exp * exp -> exp
  val ifThenExp: exp * exp  -> exp
  val whileExp : exp * exp * breakpoint -> exp
  val forExp : exp * exp * exp * exp * breakpoint -> exp
  val breakExp : breakpoint -> exp
  val assignExp : exp * exp -> exp
  val seqExp : exp list -> exp
  val letExp : exp list * exp -> exp
  val callExp : Temp.label * level * level * exp list -> exp 

  val simpleVar: access * level -> exp
  val subscriptVar: exp * exp -> exp
  val fieldVar: exp * int -> exp

  val procEntryExit: {level:level, body:exp} -> unit
  val newBreakPt : unit -> breakpoint
  val getResult : unit -> frag list
  val addFrag : frag -> unit
  val clearFrags: unit -> unit

end 

structure Translate : TRANSLATE = 
struct 
	
	structure Frame : FRAME = MipsFrame
	structure T = Tree

	datatype exp = Ex of Tree.exp
					| Nx of Tree.stm
					| Cx of Temp.label * Temp.label -> Tree.stm
	datatype level = Top | Level of {unique: unit ref, frame: Frame.frame, parent: level}
	type access = level * Frame.access
	type breakpoint = Temp.label
  type frag = Frame.frag

  val outermost = Top
  val ERROR = Ex (T.CONST 9999)

  (* HELPERS *)
  fun addIndexTuple l = 
  let fun helper (a::l,n,result) = helper(l,n+1,(a,n)::result)
      | helper ([], n, result) = result
  in 
    rev(helper(l,0,[]))
  end
  fun eqLevel(Level {unique=unique1 ,frame =_ , parent = _} , Level {unique= unique2,frame =_ , parent = _}) = (unique1 = unique2)
     | eqLevel(Top,Top) = true
     | eqLevel(_,_) = false


  fun convStaticLink (declevel, currlevel) = 
  let 
    fun helper (lev as Level{unique,frame,parent},exp) = 
      if (eqLevel(lev, declevel))
      then exp
      else helper (parent,Frame.exp(hd(Frame.formals frame)) exp)
  in 
    helper(currlevel,T.TEMP(Frame.FP))
  end

  (* STUFF *)
	fun newLevel {parent, name, formals} = Level {unique = ref (), 
												  frame = Frame.newFrame {name=name, formals = true::formals},
												  parent = parent}
	fun  formals (Top) = []
		| formals (lev as Level{unique,frame,parent}) = 
			(case Frame.formals(frame) 
				of [] => (ErrorMsg.impossible "No Static Link?")
				| head::formals => map (fn accs => (lev,accs)) formals)

  fun  allocLocal Top escape = ErrorMsg.impossible "Local variable cannot be allocated in this scope"
    	| allocLocal (lev as Level{unique, frame, parent}) escape = (lev,Frame.allocLocal frame escape)

  fun seq [] = T.EXP (T.CONST 0)
    | seq [s] = s
    | seq (h::l) = T.SEQ(h,seq l)

  val newBreakPt = Temp.newlabel

  (* EXP -> Tree exp/stm *)
  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Temp.newtemp ()
            val t = Temp.newlabel ()
            val f = Temp.newlabel ()
        in
          T.ESEQ (seq [T.MOVE (T.TEMP r, T.CONST 1),
                       genstm (t, f),
                       T.LABEL f,
                       T.MOVE (T.TEMP r, T.CONST 0),
                       T.LABEL t],
                  T.TEMP r)
        end
   	| unEx (Nx (T.EXP e)) = e
    | unEx (Nx s) = T.ESEQ (s, T.CONST 0)

  fun unNx (Ex e) = T.EXP e
    | unNx (Nx n) = n
    | unNx cx = unNx (Ex (unEx cx))

  fun unCx (Ex (T.CONST 0)) = (fn (l,r) => T.JUMP (T.NAME l,[r]))
    | unCx (Ex (T.CONST 1)) = (fn (l,r) => T.JUMP (T.NAME l,[r]))
    | unCx (Ex e) = (fn(l,r) => T.CJUMP(T.EQ,T.CONST 1, e, l, r))
    | unCx (Nx n) = ErrorMsg.impossible "Cannot unCx an Nx"
    | unCx (Cx c) = c


  (* VARIABLES *)
  fun simpleVar ((parentLevel,access):access, currentLevel) =
    Ex(Frame.exp (access) (convStaticLink(parentLevel,currentLevel)))

  fun subscriptVar (varexp, subexp) = 
    let
      val var = unEx varexp
      val sub = unEx subexp
    in
        Ex(T.MEM(
          T.BINOP(T.PLUS,var,
          T.BINOP(T.MUL,sub,
          T.CONST(Frame.wordsize)))))
    end

  fun fieldVar (varexp, offset) =
    let 
      val var = unEx varexp
    in
      Ex(T.MEM(T.BINOP(T.PLUS, var, 
         T.BINOP(T.MUL, T.CONST(offset), 
          T.CONST(Frame.wordsize)))))
    end

  fun relopCxHelper (relop,exp1,exp2) =  
  let 
    fun s1(t,f) = T.CJUMP(relop,unEx exp1, unEx exp2,t,f)
  in 
    Cx s1
  end 
  (* EXPRESSIONS *)
  fun nilExp () = Ex (T.CONST (0)) 

  fun whileExp (condexp, bodyexp, doneLabel) =
      let
        val condLabel = Temp.newlabel()
        val cond = unCx condexp
        val bodyLabel = Temp.newlabel()
        val body = unNx bodyexp
      in
        Nx (seq [T.LABEL condLabel,
                 cond(bodyLabel,doneLabel), 
                 T.LABEL bodyLabel,
                 body,
                 T.JUMP (T.NAME condLabel, [condLabel]),
                 T.LABEL doneLabel])
      end

  fun forExp (varexp, loexp, hiexp, bodyexp, breakLabel) =
      let
        val bodyLabel = Temp.newlabel ()
        val incrLabel = Temp.newlabel ()
        val var = unEx varexp
        val lo = unEx loexp
        val hi = unEx hiexp
        val body = unNx bodyexp
      in
        Nx (seq [T.MOVE (var, lo),
                 T.CJUMP (T.LE, var, hi, bodyLabel, breakLabel),
                 T.LABEL bodyLabel,
                 body,
                 T.CJUMP (T.LT, var, hi, incrLabel, breakLabel),
                 T.LABEL incrLabel,
                 T.MOVE (var, T.BINOP (T.PLUS, var, T.CONST (1))),
                 T.JUMP (T.NAME bodyLabel, [bodyLabel]),
                 T.LABEL breakLabel])
      end

  fun breakExp breakLabel = 
    Nx (T.JUMP (T.NAME breakLabel, [breakLabel]))

  fun assignExp (leftexp, rightexp) = 
    Nx (T.MOVE (unEx leftexp, unEx rightexp))

  fun seqExp [] = Ex (T.CONST (0))
    | seqExp [e] = e
    | seqExp (e :: l) =
        Ex(T.ESEQ(unNx e, unEx (seqExp l)))

  fun letExp (decsexp, bodyexp) = seqExp(rev(bodyexp::rev(decsexp)))



   (* LITERALS *)
   fun intLiteral n = Ex (T.CONST n)

   fun stringLiteral str = 
   		let 
        val label = Temp.newlabel()
        val strfrag = Frame.STRING(label, str)
   		in
        Frame.addFrag(strfrag);
   			(*Frame.frags := Frame.STRING(label, str)::(!(Frame.frags));*)
   			Ex (T.NAME label)
   		end

  fun add (exp1 , exp2) =  Ex (T.BINOP(T.PLUS,unEx exp1, unEx exp2))
  fun minus (exp1 , exp2) =  Ex (T.BINOP(T.MINUS,unEx exp1, unEx exp2))
  fun mult (exp1 , exp2) =  Ex (T.BINOP(T.MUL,unEx exp1,unEx exp2))
  fun divide (exp1 , exp2) =  Ex (T.BINOP(T.DIV,unEx exp1,unEx exp2))
  fun eq (exp1 , exp2) =  relopCxHelper(T.EQ,exp1,exp2)
  fun neq (exp1 , exp2) =  relopCxHelper(T.NE,exp1,exp2)
  fun lt (exp1 , exp2) = relopCxHelper(T.LT,exp1,exp2)
  fun gt (exp1 , exp2) =  relopCxHelper(T.GT,exp1,exp2)
  fun le (exp1 , exp2) =  relopCxHelper(T.LE,exp1,exp2)
  fun ge (exp1 , exp2) =  relopCxHelper(T.GE,exp1,exp2)


 (* do this later *)
  fun stringEq (exp1 , exp2) =  Ex (Frame.externalCall("stringEqual", [unEx exp1,unEx exp2]))
  fun stringNeq (exp1 , exp2) =  
  let
    val isEqExp = Frame.externalCall("stringEqual", [unEx exp1,unEx exp2])
    val complement = eq(Ex isEqExp,Ex (T.CONST 0))
  in
    complement
  end

(* make this better *)
  fun ifExp (exp1,exp2,exp3) = 
  let
    val r = Temp.newtemp()
    val t = Temp.newlabel()
    val f = Temp.newlabel()
    val join = Temp.newlabel()
    fun allocExp (e,label) = seq([T.LABEL(label),T.MOVE(T.TEMP(r),e),T.JUMP(T.NAME(join),[join])])
    val e1 = unCx exp1
    val e2 = allocExp(unEx exp2 , t)
    val e3 = allocExp(unEx exp3 , f)
    val cjmp = e1 (t,f)
    val resultseq = seq([cjmp,e2,e3,T.LABEL(join)])
  in
    Ex(T.ESEQ(resultseq,T.TEMP(r)))
  end

  fun ifThenExp (exp1,exp2) = 
  let
    val t = Temp.newlabel()
    val join = Temp.newlabel()
    fun allocExp (e,label) = seq([T.LABEL(label),e,T.JUMP(T.NAME(join),[join])])
    val e1 = unCx exp1
    val thenstm = allocExp(unNx exp2 , t)
    val cjmp = e1 (t,join)
    val resultseq = seq([cjmp,thenstm,T.LABEL(join)])
  in
    Nx resultseq
  end


  fun recordExp l = 
  let 
    val exp_with_indexes = addIndexTuple l
    val n = length l
    val r = Temp.newtemp()
    fun allocField (exp,n) = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(r), 
         T.CONST(n*Frame.wordsize))),unEx exp)
    val explist = map allocField exp_with_indexes
    val seqoflist = seq explist
    val extCall = T.MOVE(T.TEMP(r), T.CALL(T.NAME(Temp.namedlabel("malloc")),[T.CONST(n*Frame.wordsize)]))
    val result = T.ESEQ(T.SEQ(extCall,seqoflist),T.TEMP(r))
  in
    Ex result
  end

  fun arrayExp  (sizeexp, initexp) = 
    let
      val r = Temp.newtemp()
      val arrayArgs = [unEx sizeexp, unEx initexp]
      val arrayAlloc = T.MOVE(T.TEMP(r),Frame.externalCall("initArray",arrayArgs))
    in 
      Ex (T.ESEQ(arrayAlloc,T.TEMP(r)))
    end

  fun callExp (label, currlevel, funclevel as Level {unique,frame,parent} , args) = 
    let
      val staticlink = convStaticLink(parent,currlevel)
      val args' = map unEx args
    in
      Ex (T.CALL(T.NAME label,staticlink::args'))
    end
  | callExp (label,currlevel,funclevel as Top, args) = 
    let
      val args' = map unEx args
    in
      Ex (Frame.externalCall(Symbol.name label,args'))
    end

  fun procEntryExit {level=Level {unique, frame, parent}, body} =
      let
        val bodyexp = T.MOVE (T.TEMP Frame.RV, unEx body)
        val body' = Frame.procEntryExit1(frame, bodyexp)
        val newfrag = Frame.PROC {body = body', frame = frame} 
      in
        Frame.addFrag(newfrag)
      end
      | procEntryExit _ = ErrorMsg.impossible "Cannot create procEntryExit at Top level"

    fun getResult () = Frame.getResult()

    fun addFrag f = Frame.addFrag f

    fun clearFrags () = Frame.clearFrags()


 




	





end