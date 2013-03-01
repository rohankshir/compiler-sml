signature SEMANT = 
sig 
	val transProg : Absyn.exp -> unit
end






structure Semant :> SEMANT = 
struct 
	structure A = Absyn
	structure E = Env
	structure S = Symbol
	structure Translate = struct type exp = unit end
	type venv =  E.enventry Symbol.table
	type tenv = Types.ty Symbol.table
	type expty = {exp: Translate.exp , ty: Types.ty}

fun lookup (tenv,s,pos) = case Symbol.look(tenv,s) of 
					SOME ty => ty
				|   NONE => (ErrorMsg.error pos "Invalid type";Types.INT)
					
fun checkInt ({exp,ty},pos) = 
			case ty of Types.INT => ()
				| _ => ErrorMsg.error pos "integer required"

fun checkString ({exp,ty},pos) = 
			case ty of Types.STRING => ()
				| _ => ErrorMsg.error pos "string required"

fun checkComparable ({exp,ty} , expty2 , pos) = 
			case ty of 
				  Types.INT => checkInt (expty2,pos)
				| Types.STRING => checkString (expty2,pos)
				| _ => ErrorMsg.error pos "string or integer required"

(* ASK HILTON about array and record type checking*)
(*
fun checkEqualable ({exp=_,ty=ty1},{()),ty = ty2},pos ) = 
			case ty1 of Types.INT => checkInt ({(),ty2},pos)
				| Types.STRING => checkString ({(),ty2},pos)
				| Types.ARRAY(arrayTy,unique) => ()
				| Types.RECORD l => ()
				| _ => ErrorMsg.error pos "string or integer required"
*)
fun transExp (venv, tenv) = 
	let fun trexp (A.OpExp {left, oper, right, pos}) = 
			if 
				oper = A.PlusOp orelse oper = A.MinusOp orelse oper = A.TimesOp orelse oper = A.DivideOp 
			then 
					(checkInt(trexp left, pos); 
					checkInt(trexp right, pos);
					{exp = (),ty=Types.INT})
			else if oper = A.EqOp orelse oper = A.NeqOp orelse oper = A.GeOp orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.LtOp 
					
			then
					(checkComparable(trexp left, trexp right, pos); 
					{exp = (),ty=Types.INT})
			else
        			(ErrorMsg.error pos "error";
        			{exp=(), ty=Types.INT})
        | 	trexp (A.IntExp i) = {exp=(),ty=Types.INT}
        |	trexp (A.StringExp (s,pos)) = {exp=(),ty=Types.STRING}
		in 
			trexp 
		end


fun transTy (tenv, A.NameTy(s,pos)) = lookup(tenv,s,pos)
	| transTy(tenv, A.RecordTy l) = 
		let 
			fun convFieldToTuple {name,escape,typ,pos} = (name,lookup(tenv,typ,pos))
			val tupleList = map convFieldToTuple l
		in
			Types.RECORD(tupleList,ref ()) (*ASK HILTON*)
		end
	| transTy (tenv, A.ArrayTy(s,pos)) = Types.ARRAY(lookup(tenv,s,pos),ref ())



fun transDec (venv,tenv,A.VarDec{name,escape,typ=NONE,init,pos}) = 
		let val {exp,ty} = transExp(venv,tenv) init
		in 
			{tenv=tenv,
			venv = S.enter(venv,name,E.VarEntry{ty = ty})}
		end
|	transDec (venv,tenv,A.VarDec{name,escape,typ=SOME (symbol,pos),init,pos = varpos}) =
		let val {exp,ty} = transExp(venv,tenv) init
			val ty2 = lookup (tenv,symbol,pos)			
		in 
			if ty = ty2
			then
			{tenv=tenv,
			venv = S.enter(venv,name,E.VarEntry{ty = ty})}
			else
			(ErrorMsg.error pos "Mismatching types"; {tenv=tenv,  (*ASK HILTON*)
			venv = S.enter(venv,name,E.VarEntry{ty = ty})})
		end
|	transDec (venv,tenv,A.TypeDec[{name,ty,pos}]) = 
		{venv = venv,
		tenv = S.enter(tenv,name,transTy(tenv,ty))}
| 	transDec (venv,tenv,A.FunctionDec[{name,params,result=SOME(rt,pos'),body,pos}]) =
		let val SOME(result_ty) = S.look(tenv,rt)
			fun transparam{name,escape,typ,pos} = 
				case S.look(tenv,typ)
				 of SOME t => {name=name,ty=t}


			val params' = map transparam params
			val venv' = S.enter(venv,name,E.FunEntry{formals=map #ty params',result=result_ty})
			fun enterparam ({name,ty},venv) = S.enter(venv,name,E.VarEntry{ty=ty})
			val venv'' = foldl enterparam venv' params' 
		in transExp(venv'',tenv) body;
			{venv=venv',tenv=tenv}
		end


fun transProg ast = 
	let 
		val {exp=result,ty=_} = transExp (E.base_venv,E.base_tenv) ast
	in 
		result
	end

end