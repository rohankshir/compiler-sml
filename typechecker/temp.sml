datatype x = 
	F of unit -> x list
  | N of int

  fun f () = [N(32),F(f),N(5)]