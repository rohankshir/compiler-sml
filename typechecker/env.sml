signature ENV = 
sig 	
	type access
	type ty 
	datatype enventry = VarEntry of {ty:ty}
					  | FunEntry of {formals: ty list, result: ty}
	val base_tenv : ty Symbol.table (*predefined types*)
	val base_venv : enventry Symbol.table (*predefined functions*)
end

structure Env :> ENV = 
	struct

	open Symbol
	type ty = Types.ty
	type access = unit ref

	fun base_tenv_init () = 
	let
		val table = Symbol.empty
	in 
		table = Symbol.enter (table, symbol "print", FunEntry{formals:[Types.STRING],result: Types.UNIT}); 
		table = Symbol.enter (table, symbol "flush", FunEntry{formals:[Types.UNIT],result: Types.UNIT}); (* and so on... *)
		table
	end

	val base_venv = base_tenv_init ()

end