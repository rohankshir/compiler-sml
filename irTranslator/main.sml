structure Main = 
struct
	structure Frame : FRAME = MipsFrame
	fun main fileName = Frame.printFragList (Semant.transProg(Parse.parse(fileName)))
end