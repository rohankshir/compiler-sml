signature FRAME =
sig type frame
    type access
    type register

    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list  
    val allocLocal : frame -> bool -> access
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall: string * Tree.exp list -> Tree.exp

    val tempMap: register Temp.Table.table
    val registerToString: Temp.temp -> register
    val calldefs: Temp.temp list
    val FP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val SP : Temp.temp
    val ZERO : Temp.temp
    val GP : Temp.temp
    val wordsize: int
    val numArgs: int
    val argregs: Temp.temp list
    val registers: register list
    val colorable: register list
    val machineTemps: Temp.temp list
    val string: Tree.label * string -> string


    datatype frag = PROC of {body: Tree.stm, frame: frame}
    				| STRING of Temp.label * string
    (*val frags: frag list ref*)
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}

    val addFrag : frag -> unit
    val getResult : unit -> frag list
    val printFragList : frag list -> unit
    val clearFrags: unit -> unit

end