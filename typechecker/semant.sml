signature SEMANT = 
sig 
	val transProg : Absyn.exp -> unit
end






structure Semant :> SEMANT = 
struct 
	structure A = Absyn
	structure E = Env
	structure Translate = struct type exp = unit end
	type venv =  E.enventry Symbol.table
	type tenv = ty Symbol.table


fun transExp (venv, tenv) = 
	let fun trexp (A.OpExp{left,oper = A.PlusOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		in 
			trexp 
		end


fun transProg ast = 
	let 
		val {exp=result,ty=_} = transExp (E.base_env,E.base_tenv,ast)
	in 
		result
	end

end