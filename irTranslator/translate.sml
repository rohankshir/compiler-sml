signature TRANSLATE = 
sig 
	type level 
	type access (* not the same as Fram.access *)
	type exp 
	type breakpoint
	type frag

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access

	val unEx : exp -> Tree.exp
 	val unNx : exp -> Tree.stm
 	val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

  val intLiteral : int -> exp
  val stringLiteral : string -> exp
  val simpleVar: access * level -> exp
 	val nilExp : unit -> exp
  (*val callExp : Temp.label * level * level * exp list -> exp *)



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
	val outermost = Top
	type frag = Frame.frag
	val frags = ref [] : frag list ref (* Init frag list to empty list *)



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

   fun simpleVar ((parentLevel,access):access, currentLevel) =
    Ex(Frame.exp (access) (convStaticLink(parentLevel,currentLevel)))




	fun newLevel {parent, name, formals} = Level {unique = ref (), 
												  frame = Frame.newFrame {name=name, formals = true::formals},
												  parent = parent}
	fun   formals (Top) = []
		| formals (lev as Level{unique,frame,parent}) = 
			(case Frame.formals(frame) 
				of [] => (ErrorMsg.impossible "No Static Link?")
				| head::formals => map (fn accs => (lev,accs)) formals)

	 fun  allocLocal Top escape = ErrorMsg.impossible "Local variable cannot be allocated in this scope"
    	| allocLocal (lev as Level{unique, frame, parent}) escape = (lev,Frame.allocLocal frame escape)



  fun seq [] = T.EXP (T.CONST 0)
    | seq [s] = s
    | seq (h::l) = T.SEQ(h,seq l)


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


   fun nilExp () = Ex (T.CONST (0)) 

   fun intLiteral n = Ex (T.CONST n)

   fun stringLiteral str = 
   		let 
   			val label = Temp.newlabel() 
   		in
   			frags := Frame.STRING(label, str)::(!frags);
   			Ex (T.NAME label)
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