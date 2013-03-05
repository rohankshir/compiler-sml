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
	val nestLevel = ref 0

fun lookup (tenv,s,pos) = case Symbol.look(tenv,s) of 
					SOME ty => ty
				|   NONE => (ErrorMsg.error pos "Invalid type";Types.UNIT)
					
fun checkInt ({exp,ty},pos) = 
			case ty of Types.INT => ()
				| _ => ErrorMsg.error pos "integer required"

fun checkUnit ({exp,ty},pos) = 
			case ty of Types.UNIT => ()
				| _ => ErrorMsg.error pos "Expression must return no value"

fun checkEqualTypes (ty1,ty2,pos) =
			case (ty1,ty2) of 
				(Types.RECORD(_,ref1), Types.RECORD(_,ref2)) => 
				(if ref1=ref2 then () else (ErrorMsg.error pos "Mismatching record types"))
				| (Types.ARRAY(_,ref1), Types.ARRAY(_,ref2)) => 
				(if ref1=ref2 then () else (ErrorMsg.error pos "Mismatching array types"))
				|(Types.INT,Types.INT) => ()
				|(Types.STRING,Types.STRING) => ()
				|(_,_)=> (ErrorMsg.error pos "Mismatching  types")
fun checkString ({exp,ty} ,pos) = 
			case ty of Types.STRING => ()
				| _ => ErrorMsg.error pos "string required"

fun checkComparable ({exp,ty} , expty2 , pos) = 
			case ty of 
				  Types.INT => checkInt (expty2,pos)
				| Types.STRING => checkString (expty2,pos)
				| _ => ErrorMsg.error pos "string or integer required"


fun checkEqualable ({exp=_,ty=ty1}, {exp=_,ty=ty2}, pos) = 
			case (ty1,ty2) of 
				(Types.NIL,Types.RECORD(_)) => ()
				| (Types.RECORD(_), Types.NIL) => ()
				| (Types.RECORD(_,ref1), Types.RECORD(_,ref2)) => 
						(if ref1=ref2 then () else (ErrorMsg.error pos "error"))
				| (ty1, ty2) => checkComparable({exp=(),ty=ty1},{exp=(),ty=ty2},pos)

fun transExp (venv, tenv) = 
	let fun trexp (A.VarExp v) = trvar v 									(* VarExp *)
			| 	trexp (A.NilExp) = {exp = (), ty = Types.NIL}				(* NilExp *)
			| 	trexp (A.IntExp i) = {exp=(),ty=Types.INT}					(* IntExp *)
        	|	trexp (A.StringExp (s,pos)) = {exp=(),ty=Types.STRING}		(* StringExp *)
        	|	trexp (A.CallExp {func, args, pos}) = 						(* CallExp *)
        		(case Symbol.look(venv,func) of 
        				SOME (E.FunEntry {formals, result}) => {exp = (), ty = result}
        			| 	NONE => (ErrorMsg.error pos "undefined function"	; {exp = (), ty = Types.UNIT}))

			|	trexp (A.OpExp {left, oper, right, pos}) = 					(* OpExp *)
				if 
					oper = A.PlusOp orelse oper = A.MinusOp orelse oper = A.TimesOp orelse oper = A.DivideOp 
				then 
						(checkInt(trexp left, pos); 
						checkInt(trexp right, pos);
						{exp = (),ty=Types.INT})
				else if 
					oper = A.GeOp orelse oper = A.LeOp orelse oper = A.GtOp orelse oper = A.LtOp 		
				then
					(checkComparable(trexp left, trexp right, pos); 
					{exp = (),ty=Types.INT})
				else if 
					oper = A.EqOp orelse oper = A.NeqOp 
				then 
					(checkEqualable(trexp left, trexp right, pos); 
					{exp = (),ty=Types.INT})
				else
        			(ErrorMsg.error pos "error";
        			{exp=(), ty=Types.INT})


        	(* RecordExp *)
        									
        	| trexp (A.SeqExp l) = 											(* SeqExp *)
        		let
        			fun seqHelper [(exp,pos)] = trexp exp
        				| seqHelper ((exp,pos)::tail) = (trexp exp; seqHelper tail)
        		in 
        			seqHelper l
        		end

        	| trexp (A.AssignExp {var,exp,pos} ) =						(* AssignExp *)
        		let
        			val {exp=_,ty=varTy} = trvar var
        			val {exp=_,ty = eTy} = trexp exp
        			val () = checkEqualTypes(varTy,eTy,pos)
        		in 
        			{exp=(),ty = Types.UNIT}
        		end
  
        	| trexp (A.IfExp {test, then' = thenexp, else' = NONE, pos}) =  				(* IfExp *)
        		(checkInt(trexp test,pos);
        		checkUnit(trexp thenexp,pos);
        		{exp = (),ty=Types.UNIT})
        	| trexp (A.IfExp {test, then' = thenexp, else'=SOME(elseexp), pos}) =
        		let
        		val {exp=_,ty = tythen} = trexp thenexp
        		val {exp=_,ty = tyelse} = trexp  elseexp
        		in
        		(checkInt(trexp test,pos);
        		 checkEqualTypes(tythen, tyelse,pos);
        		 {exp=(),ty=tythen})
        		end


        	| trexp (A.WhileExp {test,body,pos}) = 						(* WhileExp *)
        		(checkInt (trexp test,pos);
        		 nestLevel := !nestLevel + 1;
        		 checkUnit (trexp body,pos);
        		 nestLevel := !nestLevel - 1;	
        		 {exp=(),ty=Types.UNIT})
        	| trexp (A.ForExp {var, escape, lo, hi, body, pos}) = 		(* ForExp *)
        		let 
        			val () = checkInt (trexp lo,pos)
        			val () = checkInt (trexp hi,pos)
        			val venv' = S.enter(venv,var,E.VarEntry{ty = Types.INT})
        			val () = (nestLevel := !nestLevel + 1)
        			val {exp,ty} = transExp (venv',tenv) body
        			val () = (nestLevel := !nestLevel - 1)
        			val () = checkUnit ({exp=exp,ty=ty},pos)
        		in
        			{exp=(),ty=ty}
        		end

        	| trexp (A.BreakExp(pos)) = 				(* BreakExp *)
        		if (!nestLevel <> 0)
        		then {exp=(),ty = Types.UNIT}
        		else (ErrorMsg.error pos "Break must be within a loop"; {exp=(),ty = Types.UNIT})
        	(* LetExp *)
        	| trexp (A.LetExp{decs, body, pos}) = 
        		let 
        			val {venv = venv', tenv = tenv'} = transDecs(venv,tenv,decs)
        		in 
        			transExp(venv',tenv') body
        		end
        	(* ArrayExp *)

        	| trexp (A.ArrayExp {typ, size, init, pos})=
      			let 
          			val {exp=_, ty=tyinit}=trexp init;
      			in
      				checkInt (trexp size, pos);
        			case S.look(tenv,typ) of 
             			 NONE => (ErrorMsg.error pos "undeclared  type"; {exp=(), ty=Types.INT})
          		 		|SOME (Types.ARRAY (ty, unique))=>                  
               				(checkEqualTypes(tyinit,ty,pos);{exp=(), ty=Types.ARRAY (ty,unique)})
           	 end
           	 | trexp (e) = (PrintAbsyn.print(TextIO.stdOut, e); raise Fail("Unimplemented"))
			and trvar (A.SimpleVar(id,pos)) = 
			(case Symbol.look(venv, id)
				of SOME(E.VarEntry{ty}) => 
					{exp = (), ty=ty}
					| NONE => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name id);
									{exp = (), ty = Types.INT}))

			in 
				trexp 
			end

			


		and transTy (tenv, A.NameTy(s,pos)) = lookup(tenv,s,pos)
			| transTy(tenv, A.RecordTy l) = 
				let 
					val () = print "in transTY"
					fun convFieldToTuple {name,escape,typ,pos} = (name,lookup(tenv,typ,pos))
					val tupleList = map convFieldToTuple l
				in
					(print "returning Types.record";Types.RECORD(tupleList,ref ())) (*ASK HILTON*)
				end
			| transTy (tenv, A.ArrayTy(s,pos)) = Types.ARRAY(lookup(tenv,s,pos),ref ())


		and transDecs (venv,tenv,l) = foldl transDec {venv=venv,tenv=tenv} l

		and transDec (A.VarDec{name,escape,typ=NONE,init,pos},{venv,tenv}) = 
				let val {exp,ty} = transExp(venv,tenv) init
				in 
					{tenv=tenv,
					venv = S.enter(venv,name,E.VarEntry{ty = ty})}
				end
		|	transDec (A.VarDec{name,escape,typ=SOME (symbol,pos),init,pos = varpos}, {venv,tenv}) =
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
		|	transDec (A.TypeDec[{name,ty,pos}],{venv,tenv}) = 
				{venv = venv,
				tenv = S.enter(tenv,name,transTy(tenv,ty))}
		|	transDec (A.TypeDec l,{venv,tenv}) = 
				let
					fun addEmptyHeader (name,tenv) = S.enter (tenv,name,Types.NAME(name,ref NONE))
					val tenv' = foldl addEmptyHeader tenv (map #name l)
					fun addActualHeader ({name,ty,pos},tenv) = S.enter(tenv,name,Types.NAME(name,ref (SOME(transTy(tenv',ty)))))
					val tenv'' = foldl addActualHeader tenv l
				in
				{venv = venv,
				tenv = tenv''}
				end
		| 	transDec (A.FunctionDec[{name,params,result=SOME(rt,pos'),body,pos}],{venv,tenv}) =
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