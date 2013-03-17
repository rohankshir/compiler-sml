structure Main = 
struct
	fun main fileName = Semant.transProg(Parse.parse(fileName))
end