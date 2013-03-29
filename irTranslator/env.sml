signature ENV = 
sig 	
	type access
	datatype enventry = VarEntry of {access: Translate.access, ty:Types.ty}
					  | FunEntry of {level: Translate.level, label: Temp.label,formals: Types.ty list, result: Types.ty}
	val base_tenv : Types.ty Symbol.table (*predefined types*)
	val base_venv : enventry Symbol.table (*predefined functions*)
end

structure Env :> ENV = 
	struct

	open Symbol
	type access = Translate.access
	datatype enventry = VarEntry of {access: Translate.access, ty:Types.ty}
					  | FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}

	fun base_venv_init () = 
	let
		fun helper ((name, fentry),table ) = Symbol.enter (table, name, fentry)
	in 
		foldl helper Symbol.empty
		[
		(symbol("print"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("print"), formals=[Types.STRING], result=Types.UNIT}),
    	(symbol("flush"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("flush"), formals=[], result=Types.UNIT}),
    	(symbol("getchar"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("getchar"), formals=[], result=Types.STRING}),
    	(symbol("ord"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("ord"), formals=[Types.STRING], result=Types.INT}),
    	(symbol("chr"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("chr"), formals=[Types.INT], result=Types.STRING}),
    	(symbol("size"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("size"), formals=[Types.STRING], result=Types.INT}),
    	(symbol("substring"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("substring"), formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING}),
    	(symbol("concat"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("concat"), formals=[Types.STRING,Types.STRING], result=Types.STRING}),
    	(symbol("not"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("not"), formals=[Types.INT], result=Types.INT}),
    	(symbol("exit"), FunEntry {level = Translate.outermost, label=Temp.namedlabel("exit"), formals=[Types.INT], result=Types.UNIT})
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