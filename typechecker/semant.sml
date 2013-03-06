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
	exception Error

fun actual_ty (Types.NAME (s,ty)) = 
      		(case !ty of
      			SOME t => t
        		|NONE => raise Error (* need to change this to not throw exception *)
      			 )
    		| actual_ty t = t

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

fun checkUnit ({exp,ty},pos) = 
			case ty of Types.UNIT => ()
				| _ => ErrorMsg.error pos "Expression must return no value"

fun checkArray({exp,ty},pos) =
        	case ty of Types.ARRAY _ => ()
                 | _     => ErrorMsg.error pos "array required"

fun checkRecord({exp,ty},pos) =
        	case ty of Types.RECORD _ => ()
                 | _     => ErrorMsg.error pos "record required"

fun eqTypes (ty1,ty2) =
			case (ty1,ty2) of 
				(Types.RECORD(_,u1), Types.RECORD(_,u2)) => (u1=u2)
				| (Types.ARRAY(_,u1),Types.ARRAY(_,u2)) => 	(u1=u2)
				| (Types.NAME(_,_), Types.NAME(_,_)) => eqTypes(actual_ty ty1, actual_ty ty2)
				| (Types.NAME(_,_),_) => eqTypes(actual_ty ty1,ty2)
				| (_,Types.NAME(_,_)) => eqTypes(ty1, actual_ty ty2)
				| (Types.RECORD(_,_), Types.NIL) => true
    			| (Types.NIL, Types.RECORD(_,_)) => true
    			| (_,_) => (ty1 = ty2)
    			(* Can arrays = NIL? *)

(* eqTypeList: Type.ty list * Type.ty list -> bool *)
fun eqTypeList ([],[]) = true
       			|eqTypeList([],l) = false
        		|eqTypeList(l,[]) = false
        		|eqTypeList ([ty1],[ty2]) = eqTypes(ty1,ty2)
        		|eqTypeList (hd1::l1, hd2::l2) = eqTypes(hd1,hd2) andalso eqTypeList(l1,l2)


				    
 
  

fun transExp (venv, tenv) = 
	let fun trexp (A.VarExp v) = trvar v 									(* VarExp *)
			| 	trexp (A.NilExp) = {exp = (), ty = Types.NIL}				(* NilExp *)
			| 	trexp (A.IntExp i) = {exp=(),ty=Types.INT}					(* IntExp *)
        	|	trexp (A.StringExp (s,pos)) = {exp=(),ty=Types.STRING}		(* StringExp *)
        	|	trexp (A.CallExp {func, args, pos}) = 						(* CallExp *)
        		(case Symbol.look(venv,func) of 
        				SOME (E.FunEntry {formals, result}) => 
        					let 

        						val argtys = map #ty (map trexp args)
        					in 
        						if eqTypeList(formals, argtys) 
        						then {exp = (), ty=actual_ty result} 
        						else ((ErrorMsg.error pos "function arguments do not agree"); {exp=(),ty=Types.UNIT})
        					end
        			| 	SOME (E.VarEntry{ty}) => ((ErrorMsg.error pos "undefined function"); {exp = (), ty = Types.UNIT})	
        			| 	NONE => (ErrorMsg.error pos "undefined function"	; {exp = (), ty = Types.UNIT}))


       
			|	trexp (A.OpExp {left, oper, right, pos}) = 					(* OpExp *)

				let 
					val left' = trexp left
					val right' = trexp right
				in 
				((case (left') 
					of {exp=_,ty=Types.INT} => checkInt(right', pos)
					|  {exp=_,ty=Types.STRING} => checkString(right', pos)
					|  {exp=_,ty=Types.ARRAY(_)} =>
						(case oper 
							of A.EqOp => (checkArray(right', pos); eqTypes(#ty left', #ty right');())
							|  A.NeqOp => (checkArray(right', pos); eqTypes(#ty left', #ty right');())
							| _ => (ErrorMsg.error pos "operation not valid for ARRAYS")
						)
					| 	{exp=_,ty=Types.RECORD(_)} =>
						(case oper 
							of A.EqOp => (checkRecord(right', pos); eqTypes(#ty left', #ty right');())
							|  A.NeqOp => (checkRecord(right', pos); eqTypes(#ty left', #ty right');())
							| _ => (ErrorMsg.error pos "operation not valid for RECORDS")
						)
					| 	_ => (ErrorMsg.error pos "invalid operation")
				);
				{exp = (), ty = Types.INT})
				end

        	(* RecordExp *)
        	| trexp (A.RecordExp {fields,typ,pos}) = 
        		let 
        			val actualType = actual_ty (lookup (tenv,typ,pos)) (*add type checking for if not a record*)
        			fun findFieldType sym = 
	        			let
	        				fun helper((s,ty),t) = 
		        				if s = sym
		        				then ty
		        				else t
		        		in 
		        			(case actualType of 
		        			Types.RECORD (l,unique) => foldl helper Types.UNIT l
		        			| _ => (ErrorMsg.error pos "Not a record type"; Types.UNIT))
		        		end
		        	fun checkFieldTypes (sym,exp,pos) = 
		        		let
		        			val t = findFieldType sym
		        		in 
		        			if eqTypes(t,#ty (trexp exp))
		        			then ()
		        			else ErrorMsg.error pos "Mismatching field types"
		        		end 
		        	val () = app checkFieldTypes fields
        		in
        			{exp = (), ty = actualType}
        		end

        									
        	| trexp (A.SeqExp l) = 											(* SeqExp *)
        		let
        			fun seqHelper [(exp,pos)] = trexp exp
        				| seqHelper ((exp,pos)::tail) = (trexp exp; seqHelper tail)
        		in 
        			seqHelper l
        		end

        	| trexp (A.AssignExp {var,exp,pos} ) =						(* AssignExp *)
        		let
        			val var_ty = #ty (trvar (var))
        			val exp_ty = #ty (trexp (exp))
        		in 
        			if (eqTypes(var_ty,exp_ty))
        			then {exp = (), ty = Types.UNIT}
        			else (ErrorMsg.error pos "type mismatch in var assignment";{exp = (), ty = Types.UNIT})
        		end



  
        	| trexp (A.IfExp {test, then' = thenexp, else' = NONE, pos}) =  				(* IfExp *)
        		(checkInt(trexp test,pos);
        		checkUnit(trexp thenexp,pos);
        		{exp = (),ty=Types.UNIT})

        	| trexp (A.IfExp {test, then' = thenexp, else'=SOME(elseexp), pos}) =
        		let
        			val then_ty = #ty (trexp thenexp)
        			val else_ty = #ty (trexp  elseexp)
        		in
        			(checkInt(trexp test,pos);
        		 	if eqTypes(then_ty, else_ty) 
        		 	then {exp=(),ty=then_ty}
        		 	else (ErrorMsg.error pos "then and else expressions mus have same type"; {exp=(),ty=then_ty})
        		 	)
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
           	 
			


			and trvar (A.SimpleVar(id,pos)) = 
			(case Symbol.look(venv, id)
				of SOME(E.VarEntry{ty}) => 
					{exp = (), ty=actual_ty ty}
					| NONE => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name id);
									{exp = (), ty = Types.INT}))

            | trvar (A.FieldVar(var,id,pos)) = 
            (case trvar var
              of {exp,ty=Types.RECORD(fields,_)} =>
                (let 
                    fun idfinder (symid,_) = (symid = id)
                in
                    (case (List.find idfinder fields)
                        of SOME(_,ty) => {exp=(),ty=actual_ty ty}
                         | NONE       => (ErrorMsg.error pos ("record does not have this field" ^ Symbol.name id);
                                    {exp=(),ty=Types.UNIT}))
                    end)
               | {exp,ty} => (ErrorMsg.error pos "not a record type";
                              {exp=(), ty=Types.UNIT}))

            | trvar (A.SubscriptVar(var, exp, pos)) =
                (checkInt((trexp exp), pos);
                (case (#ty (trvar var)) of 
                Types.ARRAY(ty, _) => 
                    {exp=(), ty=actual_ty ty}
                | _ => (ErrorMsg.error pos ("not an array type");
                    {exp=(), ty=Types.UNIT})))
			in 
				trexp 
			end

			


		and transTy (tenv, A.NameTy(s,pos)) = lookup(tenv,s,pos)
			| transTy(tenv, A.RecordTy l) = 
				let 
					fun convFieldToTuple {name,escape,typ,pos} = (name,lookup(tenv,typ,pos))
					val tupleList = map convFieldToTuple l
				in
					Types.RECORD(tupleList,ref ()) (*ASK HILTON*)
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
		|	transDec (A.TypeDec l,{venv,tenv}) = 
				let
					fun addEmptyHeader (name,tenv) = (S.enter (tenv,name,Types.NAME(name,ref NONE)))
					val names = (map #name l)
					val absynTypes = (map #ty l)
					val tenv' = foldl addEmptyHeader tenv names
					fun replace(Types.NAME(n,r),ty) =  r := SOME ty 
					   | replace(_,_) = raise Fail("How is that not a NAME") 
					(*val types = map (fn n => (S.look (tenv', n)))  names *)
					fun replaceHeaders {name,ty,pos} = replace(Option.valOf(S.look (tenv',name)), transTy(tenv',ty))
					val () = app replaceHeaders  l
				in
				{venv = venv,
				tenv = tenv'}
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