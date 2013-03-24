structure MipsFrame : FRAME =
struct

  datatype access = InFrame of int | InReg of Temp.temp 
  type frame = {name: Temp.label, 
                formals: access list, 
                numLocals: int ref, 
                frameOffset: int ref
                }
  val wordsize = 4


 fun allocFormal (esc, (accs,frameOffset)) =
          (* if formal escapes, add InFrame to access list and push frameOffset down *)
          (case esc of true => (InFrame(frameOffset - wordsize)::accs, frameOffset - wordsize)
          (* otherwise, add InReg to access list and frameOffset stays same *)
                | false => (InReg(Temp.newtemp())::accs, frameOffset))

  fun newFrame{name, formals} = 
     let
            val (access_list,offset) = foldl allocFormal ([],0) formals
     in
            {name=name, formals=access_list, numLocals=ref 0, frameOffset=ref offset}
     end     
  
  fun name(f:frame) = #name f
  fun formals (f:frame) = #formals f
  

  fun allocLocal (f:frame) b = 
    (case b of true =>
            (((#numLocals f):= !(#numLocals f)+ 1);
            ((#frameOffset f) := !(#frameOffset f) - wordsize);
            InFrame(!(#frameOffset f)))
      | false =>
            (((#numLocals f):= !(#numLocals f)+ 1);
            InReg(Temp.newtemp())))

end