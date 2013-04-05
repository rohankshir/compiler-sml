structure Mips:CODEGEN = 
struct
	structure F = MipsFrame
	structure T = Tree
	structure A = Assem
	structure S = Symbol

	fun getBranchString T.EQ = "beq"
    		| getBranchString T.NE = "bne"
    		| getBranchString T.LT = "blt"
    		| getBranchString T.GT = "bgt"
    		| getBranchString T.LE = "ble"
    		| getBranchString T.GE = "bge"
    		| getBranchString _ = "WUT"

    fun getBinopString T.PLUS = "add"
    		| getBinopString T.MINUS = "sub"
    		| getBinopString T.MUL = "mult"
    		| getBinopString T.DIV = "div"
    		| getBinopString T.AND = "and"
    		| getBinopString T.OR = "or"
    		| getBinopString _ = "WUT"

	val calldefs = [F.RV,F.RA]

	fun codegen (frame) (stm : Tree.stm) : A.instr list = 
	let val ilist = ref (nil: A.instr list)
		fun emit x = ilist := x :: !ilist
		fun int i = Int.toString(i)
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

		

    	(**************** MUNCH STM *******************)
    	(* T.SEQ *)
		fun munchStm (T.SEQ(a,b)) = (munchStm a; munchStm b)

		(* T.LABEL *)
			| munchStm (T.LABEL lab) = 
		  	    emit(A.LABEL{assem= S.name(lab) ^ ":\n", lab = lab})

		(* T.JUMP *)
		  	| munchStm (T.JUMP(T.NAME(lab),l)) = 
		  		emit(A.OPER{assem = "j " ^ (S.name lab) ^ "\n",
          			src = [], 
          			dst = [], 
          			jump=SOME[Temp.namedlabel(S.name lab)]})
		  	| munchStm (T.JUMP(e,l)) = 
		  		emit(A.OPER{assem = "jr 's0\n",
		  			src = [munchExp e],
		  			dst = [],
		  			jump = SOME l})

		(* T.CJUMP *)
		  	| munchStm (T.CJUMP(relop,e1,T.CONST i,lab1,lab2)) = 
		  		emit(A.OPER{assem = (getBranchString(relop) ^ " 's0," ^ int i ^ "," ^ (S.name lab1) ^ "\n"), 
		  			src = [munchExp e1], 
		  			dst = [],
		  			jump = SOME([lab1,lab2])})
		  	| munchStm (T.CJUMP(relop,T.CONST i,e2,lab1,lab2)) = 
		  		emit(A.OPER{assem = (getBranchString(relop) ^ " 's0," ^ int i ^ "," ^ (S.name lab1) ^ "\n"), 
		  			src = [munchExp e2], 
		  			dst = [],
		  			jump = SOME([lab1,lab2])})
		  	| munchStm (T.CJUMP(relop,e1,e2,lab1,lab2)) = 
		  		emit(A.OPER{assem = (getBranchString(relop) ^ " 's0, 's1, " ^ (S.name lab1) ^ "\n"), 
		  			src = [munchExp e1, munchExp e2], 
		  			dst = [],
		  			jump = SOME([lab1,lab2])})

		(* T.MOVE *)  		
		  	| munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
		  		emit(A.OPER{assem = "sw 's1, " ^ (int i) ^ "('s0)\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})

		  	| munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) = 
		  		emit(A.OPER{assem = "sw 's1, " ^ (int i) ^ "('s0)\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})

		  	| munchStm(T.MOVE(T.MEM(T.CONST i),e2)) =
        		emit(A.OPER{assem="sw 's0, " ^ int i ^ "($zero)\n",
                    src=[munchExp e2],
                    dst=[], jump=NONE})
      
    		| munchStm(T.MOVE(T.MEM(e1),e2)) =
        		emit(A.OPER{assem="sw 's1, 's0\n",
                    src=[munchExp e1, munchExp e2],
                    dst=[], jump=NONE})

    		| munchStm(T.MOVE(T.TEMP i, e2)) =
        		emit(A.MOVE{assem="move 'd0, 's0\n",
          			src= (munchExp e2),
          			dst= i})

		(* T.EXP *)
			| munchStm (T.EXP exp) = (munchExp exp; ())
			| munchStm _ = ErrorMsg.impossible("bad munch stm")


		(**************** MUNCH EXP *******************)
		(* T.BINOP *)
			(* add immediate *)
		and munchExp(T.BINOP(T.PLUS,e1,T.CONST i)) =
          	result(fn r => emit(A.OPER{assem="addi 'd0, 's0, " ^ int i ^ "\n",
            src=[munchExp e1], dst=[r], jump=NONE}))
      	| munchExp(T.BINOP(T.PLUS,T.CONST i,e1)) =
          	result(fn r => emit(A.OPER{assem="addi 'd0, 's0, " ^ int i ^ "\n",
           	src=[munchExp e1], dst=[r], jump=NONE}))
       	
       		(* subtract immediate *)
       	| munchExp(T.BINOP(T.MINUS,e1,T.CONST i)) =
          	result(fn r => emit(A.OPER{assem="addi 'd0, 's0, " ^ int (~i) ^ "\n",
            src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp(T.BINOP(T.MINUS,T.CONST i,e1)) = 
        	result(fn r => emit(A.OPER {assem="addi 'd0, 's0, " ^ int i ^ "\n",
            src=[munchExp ((T.BINOP(T.MINUS,T.CONST 0,e1)))], dst=[r], jump=NONE}))
       	
        	(* and immediate *)
       	| munchExp(T.BINOP(T.AND,e1,T.CONST i)) =
          	result(fn r => emit(A.OPER{assem="andi 'd0, 's0, " ^ int i ^ "\n",
        	src=[munchExp e1], dst=[r], jump=NONE}))
      	| munchExp(T.BINOP(T.AND,T.CONST i,e1)) =
          	result(fn r => emit(A.OPER{assem="andi 'd0, 's0, " ^ int i ^ "\n",
            src=[munchExp e1], dst=[r], jump=NONE}))

        	(* or immediate *)
        | munchExp(T.BINOP(T.OR,e1,T.CONST i)) =
          	result(fn r => emit(A.OPER{assem="ori 'd0, 's0, " ^ int i ^ "\n",
            src=[munchExp e1], dst=[r], jump=NONE}))
      	| munchExp(T.BINOP(T.OR,T.CONST i,e1)) =
         	result(fn r => emit(A.OPER{assem="ori 'd0, 's0, " ^ int i ^ "\n",
            src=[munchExp e1], dst=[r], jump=NONE}))

        	(* rest of binops *)
      	| munchExp(T.BINOP(binop,e1,e2)) =
        	result(fn r => emit(A.OPER{assem = getBinopString(binop) ^ " 'd0, 's0, 's1\n",
        	src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
      
		(* T.MEM *)
		| munchExp (T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) = 
		 	result (fn r => emit (A.OPER
		 		{assem = "lw 'd0, " ^ int i ^ "('s0)\n",
		 		 src = [munchExp e1], dst = [r], jump = NONE}))
		| munchExp (T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) = 
		 	result (fn r => emit (A.OPER
		 		{assem = "lw 'd0, " ^ int i ^ "('s0)\n",
		 		 src = [munchExp e1], dst = [r], jump = NONE}))
		(* Can you do MINUS???????? *)
		| munchExp (T.MEM(T.CONST i)) = 
		   	result (fn r => emit(A.OPER
		   		{assem="li 'd0, " ^ int i ^ "\n", 
		   		src=[], dst=[r], jump = NONE}))
		| munchExp (T.MEM e1) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "lw 'd0, 0('s0)\n",
		   		src=[munchExp e1], dst=[r], jump = NONE}))

		(* T.TEMP *)
		| munchExp (T.TEMP t) = t

		(* T.ESEQ *)
		| munchExp (T.ESEQ (stm,exp)) = (munchStm stm; munchExp exp)

		(* T.NAME *)
		| munchExp(T.NAME label) =
        	result(fn r => emit(A.OPER{assem=("la 'd0, " ^ S.name(label)),
            	src=[], dst=[r], jump=NONE}))

		(* T.CONST *)
		| munchExp (T.CONST i) = 
			result(fn r => emit(A.OPER{assem="li 'd0, " ^ int i ^ "\n",
                src=[], dst=[r], jump=NONE}))
		| munchExp _ = ErrorMsg.impossible("bad munch exp")
		(* T.CALL *)



	in 
		rev (!ilist)
	end
end

