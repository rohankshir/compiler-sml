structure Main = 
struct
	fun main fileName = Semant.transProg(Parse.parse(fileName))


(* add code to Parse.parse a test file AND Semant.transProg the resulting absyn *)

end