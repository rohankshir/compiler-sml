signature TRANSLATE = 
sig 
	type level 
	type access (* not the same as Fram.access *)
	type exp 
	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access
end 

structure Translate : TRANSLATE = 
struct 
	
	structure Frame : FRAME = MipsFrame

	datatype exp = Ex of Tree.exp
					| Nx of Tree.stm
					| Cx of Temp.label * Temp.label -> Tree.stm

	datatype level = Top | Level of {unique: unit ref, frame: Frame.frame, parent: level}
	type access = level * Frame.access
	val outermost = Top
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
end