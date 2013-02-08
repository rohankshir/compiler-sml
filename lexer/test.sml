fun convertAscii s= 
	let
  	val ascii = valOf(Int.fromString(String.substring(s,1,size s -1)))
  	val result = if ascii < 255
    then  Char.toString(Char.chr(ascii))
    else ""
	in
	  result
	end
