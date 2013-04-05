structure FindEscape: 

sig 
	val findEscape: Absyn.exp -> unit 
end

= 

struct

	structure S = Symbol
	structure T = Types
	structure A = Absyn 

	type depth = int
	type escEnv = (depth * bool ref) Symbol.table

	(* TRAVERSE VAR *)
	fun traverseVar(env:escEnv, d:depth, s:A.var) : unit = 
		case s of 
			A.FieldVar(var,sym,pos) => traverseVar(env,d,var)
			| A.SubscriptVar(var, exp, pos) => (traverseVar(env,d,var); traverseExp(env,d,exp))
			| A.SimpleVar(sym, pos) =>
				case S.look(env,sym) of 
					SOME (depth',escape) => if (depth' < d) then (escape:=true) else ()
					|_ => ()

	(* TRAVERSE EXP *)
	and traverseExp(env:escEnv, d:depth, s:A.exp) : unit = 
		let 
			fun travexp exp = traverseExp (env,d,exp)
		in 
			case s of
        		A.VarExp var => traverseVar (env, d, var)
      			| A.CallExp {func, args, pos} => (app travexp args)
      			| A.OpExp {left, oper, right, pos} => (travexp left; travexp right)
      			| A.RecordExp {fields, typ, pos} => (app travexp (map #2 fields))
      			| A.SeqExp exps => (app travexp (map #1 exps))
      			| A.AssignExp {var, exp, pos} => (traverseVar (env, d, var); travexp exp)
      			| A.IfExp {test, then', else', pos} =>
        			(travexp test; travexp then';
          			case else' of
            			SOME exp => travexp exp
          				|_ => ())
      			| A.WhileExp {test, body, pos} => (travexp test; travexp body)
      			| A.ForExp {var, escape, lo, hi, body, pos} => 
      				(travexp lo; travexp hi; 
      				traverseExp (S.enter (env, var, (d, escape)), d, body))
      			| A.LetExp {decs, body, pos} => traverseExp (traverseDecs (env, d, decs), d, body)
      			| A.ArrayExp {typ, size, init, pos} => (travexp size; travexp init)
      			| _ => ()
    end 


	(* TRAVERSE DECS *)
	and traverseDecs (env, d, []) = env
    	| traverseDecs (env, d, h::decs) =
      let
        fun traverseFundec {name, params, result, body, pos} =
        	let 
        		fun addToEnv ({name, escape, typ, pos},env) = 
        			(escape:=false; S.enter (env, name, (d + 1, escape)))
        		val env' = foldl addToEnv env params
         	in 
         		traverseExp (env', d + 1, body)
         	end
      in
        case h of
        	A.FunctionDec fundeclist => (app traverseFundec fundeclist; env)
        	| A.TypeDec tydeclist => traverseDecs (env, d, decs)
        	| A.VarDec {name, escape, typ, init, pos} => 
        		(escape:=false; traverseDecs (S.enter (env, name, (d, escape)), d, decs))
      end

	fun findEscape(prog: A.exp) : unit = traverseExp(S.empty, 0, prog)
end