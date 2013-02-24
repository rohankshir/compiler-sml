datatype Wrapper = Int    of int * int
                 | String of string * string
fun firstStr(Int (0,0),    n:string) = n
  | firstStr(String (b,a), n:string) = if b>n then n else b
  | firstStr(_) = 0


