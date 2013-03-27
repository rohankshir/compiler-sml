structure Main = 
struct
	fun main fileName = Printtree.printtree (TextIO.stdOut ,Semant.transProg(Parse.parse(fileName)))
end