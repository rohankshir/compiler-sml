structure Mips:CODEGEN = 
struct
	structure FRAME = MipsFrame
	structure T = Tree
	structure A = Assem

	fun codegen (frame) (stm : Tree.stm) : A.instr list = 
	let val ilist = ref (nil: A.instr list)
		fun emit x = ilist := x :: !ilist
		fun int i = Int.toString(i)
		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

		fun munchStm (T.SEQ(a,b)) = (munchStm a; munchStm b)
		  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
		  		emit(A.OPER{assem = "STORE M['s0 + " ^ int i ^ "] <- 's1\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})
		  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) = 
		  		emit(A.OPER{assem = "STORE M['s0 + " ^ int i ^ "] <- 's1\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})
		  | munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) = 
		  		emit(A.OPER{assem = "MOVE M['s0] <- M['s1]\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})
		  | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) = 
		  		emit(A.OPER{assem = "STORE M[r0 + " ^ int i ^ "] <- 's0\n", 
		  			src = [munchExp e2],
		  			dst = [], jump = NONE})
		  | munchStm (T.MOVE(T.MEM(e1), e2)) = 
		  		emit(A.OPER{assem = "STORE M['s0] <- 's1\n", 
		  			src = [munchExp e1, munchExp e2],
		  			dst = [], jump = NONE})
		  | munchStm (T.MOVE(T.TEMP i, e2)) = 
		  		emit(A.OPER{assem = "ADD  'd0 <- 's0 + r0\n", 
		  			src = [munchExp e2],
		  			dst = [i], jump = NONE})
		  | munchStm (T.LABEL lab) = 
		  	    emit(A.LABEL{assem= Symbol.name(lab) ^ ": \n", lab = lab})
		 and munchExp (T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) = 
		 	result (fn r => emit (A.OPER
		 		{assem = "LOAD 'd0 <- M['s0 + " ^ int i ^ "]\n",
		 		 src = [munchExp e1], dst = [r], jump = NONE}))
		   | munchExp (T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) = 
		 	result (fn r => emit (A.OPER
		 		{assem = "LOAD 'd0 <- M['s0 + " ^ int i ^ "]\n",
		 		 src = [munchExp e1], dst = [r], jump = NONE}))
		   | munchExp (T.MEM(T.CONST i)) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "LOAD 'd0 <- M[r0+" ^ int i ^ "]\n",
		   		src = [], dst = [r], jump = NONE}))
		   | munchExp (T.MEM(e1)) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "LOAD 'd0 <- M['s0+0]\n",
		   		src = [munchExp e1], dst = [r], jump = NONE}))
		   | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "ADDI 'd0 <- 's0+" ^ int i ^ "\n",
		   		src = [munchExp e1], dst = [r], jump = NONE}))
		   | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "ADDI 'd0 <- 's0+" ^ int i ^ "\n",
		   		src = [munchExp e1], dst = [r], jump = NONE}))
		   | munchExp (T.CONST i) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "ADDI 'd0 <- r0+" ^ int i ^ "\n",
		   		src = [], dst = [r], jump = NONE}))
		   | munchExp (T.BINOP(T.PLUS, e1, e2)) = 
		   	result (fn r => emit(A.OPER 
		   		{assem = "ADD 'd0 <- 's0+'s1\n",
		   		src = [munchExp e1, munchExp e2], dst = [r], jump = NONE}))
		   | munchExp (T.TEMP t) = t

	in 
		rev (!ilist)
	end
end

