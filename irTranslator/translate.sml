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

  val stringeq:  exp * exp -> exp
  val stringneq:  exp * exp -> exp
  val stringlt:  exp * exp -> exp
  val stringgt:  exp * exp -> exp
  val stringle:  exp * exp -> exp
  val stringge:  exp * exp -> exp



 	val nilExp : unit -> exp
  val recordExp: exp list -> exp
  val arrayExp: exp * exp -> exp
  val ifExp: exp * exp * exp -> exp
  (*val callExp : Temp.label * level * level * exp list -> exp *)

  val simpleVar: access * level -> exp
  val subscriptVar: exp * exp -> exp
  val fieldVar: exp * int -> exp

	val procEntryExit: {level: level, body: exp} -> unit
 	val getResult : unit -> frag list



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
	val frags = ref [] : frag list ref (* Init frag list to empty list *)
  val outermost = Top
  val ERROR = Ex (T.CONST 9999)

  (* HELPERS *)
  fun addIndexTuple l = 
  let fun helper (a::l,n,result) = helper(l,n+1,(a,n)::result)
      | helper ([], n, result) = result
  in 
    helper(l,0,[])
  end
  fun eqLevel(Level {unique=unique1 ,frame =_ , parent = _} , Level {unique= unique2,frame =_ , parent = _}) = (unique1 = unique2)
     | eqLevel(Top,Top) = true
     | eqLevel(_,_) = false

  fun convStaticLink (parent, child) = 
    let
      fun helper (lev as Level {unique = u,frame = f ,parent = parentLevel},exp) = 
        if (eqLevel(lev,parent))
        then exp
        else helper(parentLevel, Frame.exp(hd(Frame.formals f)) (exp))
    in helper(child,T.TEMP(Frame.FP))
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
        Ex(T.ESEQ(T.MOVE(T.TEMP(Temp.newtemp()),
          T.BINOP(T.PLUS,var,
          T.BINOP(T.MUL,sub,
          T.CONST(Frame.wordsize)))),
          T.MEM(T.TEMP(Temp.newtemp()))))
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


   (* LITERALS *)
   fun intLiteral n = Ex (T.CONST n)

   fun stringLiteral str = 
   		let 
   			val label = Temp.newlabel() 
   		in
   			frags := Frame.STRING(label, str)::(!frags);
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
  fun stringeq (exp1 , exp2) =  relopCxHelper(T.EQ,exp1,exp2)
  fun stringneq (exp1 , exp2) =  relopCxHelper(T.NE,exp1,exp2)
  fun stringlt (exp1 , exp2) = relopCxHelper(T.LT,exp1,exp2)
  fun stringgt (exp1 , exp2) =  relopCxHelper(T.GT,exp1,exp2)
  fun stringle (exp1 , exp2) =  relopCxHelper(T.LE,exp1,exp2)
  fun stringge (exp1 , exp2) =  relopCxHelper(T.GE,exp1,exp2)

  fun ifExp (exp1,exp2,exp3) = exp1


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
 (*fun callExp (label, currlevel, calllevel, args) = 
    case calllevel of 
      Level {unique, frame, parent = parent as Level _ } => Ex (T.CALL (T.NAME label, ))
      (*FINISH THIS***)

*)





	fun procEntryExit {level=Level {unique, frame, parent}, body} =
      let
        val body' = Frame.procEntryExit1(frame, unNx body)
        val frag = Frame.PROC {body = body', frame = frame} 
      in
        frags := (frag :: !frags)
      end
      | procEntryExit _ = ErrorMsg.impossible "Cannot create procEntryExit at Top level"

 fun getResult () = !frags





end