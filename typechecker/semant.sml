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

fun checkInt ({exp=_,ty=ty'},pos) = 
			case ty' of Types.INT => ()
				| _ => error pos "integer required"
fun checkString ({exp=_,ty=ty'},pos) = 
			case ty' of Types.STRING => ()
				| _ => error pos "integer required"

fun checkComparable ({exp=_,ty=ty1},{()),ty = ty2},pos ) = 
			case ty1 of Types.INT => checkInt ({(),ty2},pos)
				| Types.STRING => checkString ({(),ty2},pos)
				| _ => error pos "string or integer required"


(* ASK HILTON about array and record type checking*)
fun checkEqualable ({exp=_,ty=ty1},{()),ty = ty2},pos ) = 
			case ty1 of Types.INT => checkInt ({(),ty2},pos)
				| Types.STRING => checkString ({(),ty2},pos)
				| Types.ARRAY(arrayTy,unique) => ()
				| Types.RECORD l: (symbol = s,ty = recordTy) list => ()
				| _ => error pos "string or integer required"

fun transExp (venv, tenv) = 
	let fun trexp (A.OpExp{left,oper = A.PlusOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.MinusOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.TimesOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.DivideOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.EqOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.NeqOp, right, pos}) = 
					(checkInt(trexp left, pos); 
					 checkInt(trexp right, pos);
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.GeOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.LeOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.GtOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.LtOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.EqOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
					 {exp = (),ty=Types.INT})
		| trexp (A.OpExp{left,oper = A.NeqOp, right, pos}) = 
					(checkComparable(trexp left, trexp right, pos); 
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