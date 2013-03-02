signature ENV = 
sig 	
	type access
	datatype enventry = VarEntry of {ty:Types.ty}
					  | FunEntry of {formals: Types.ty list, result: Types.ty}
	val base_tenv : Types.ty Symbol.table (*predefined types*)
	val base_venv : enventry Symbol.table (*predefined functions*)
end

structure Env :> ENV = 
	struct

	open Symbol
	type access = unit
	datatype enventry = VarEntry of {ty:Types.ty}
					  | FunEntry of {formals: Types.ty list, result: Types.ty}

	fun base_venv_init () = 
	let
		fun helper ((name, fentry),table ) = Symbol.enter (table, name, fentry)
	in 
		foldl helper Symbol.empty
		[
		(symbol("print"), FunEntry {formals=[Types.STRING], result=Types.UNIT}),
    	(symbol("flush"), FunEntry {formals=[Types.UNIT], result=Types.UNIT}),
    	(symbol("getchar"), FunEntry {formals=[Types.UNIT], result=Types.STRING}),
    	(symbol("ord"), FunEntry {formals=[Types.STRING], result=Types.INT}),
    	(symbol("chr"), FunEntry {formals=[Types.INT], result=Types.STRING}),
    	(symbol("size"), FunEntry {formals=[Types.STRING], result=Types.INT}),
    	(symbol("substring"), FunEntry {formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING}),
    	(symbol("concat"), FunEntry {formals=[Types.STRING,Types.STRING], result=Types.STRING}),
    	(symbol("not"), FunEntry {formals=[Types.INT], result=Types.INT}),
    	(symbol("exit"), FunEntry {formals=[Types.INT], result=Types.UNIT})
  		]
	end

	fun base_tenv_init () = 
	let
		fun enter ((name, ty),table) = Symbol.enter (table, name, ty)
	in 
		foldl enter empty
		[
		(symbol("int"), Types.INT),
    	(symbol("string"), Types.STRING)
    	]

	end

	val base_venv = base_venv_init ()
	val base_tenv = base_tenv_init ()

end