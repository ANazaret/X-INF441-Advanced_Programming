(* ==================================================================== *)
(* Modules descriptions:                                                *)
(* - VM      : opcodes + VM state definitions + opcode evaluation       *)
(* - HiAsm   : hi-opcodes definition + labels resolution                *)
(* - PP      : opcode pretty-printing                                   *)
(* - Dump    : functions from dumping VM state                          *)
(* - Parser  : glue between Vmlib ASM parser and hi-opcodes             *)
(* ==================================================================== *)

(* ==================================================================== *)
module SMap = Map.Make(String)

(* ==================================================================== *)
let odfl  x = function None -> x | Some x -> x
let omap  f = function None -> None | Some x -> Some (f x)
let obind f = function None -> None | Some x -> f x
let ofold f = fun acc -> function None -> acc | Some x -> f x acc

(* ==================================================================== *)
type word = int64

(* ==================================================================== *)
module VM = struct
  (* ------------------------------------------------------------------ *)
  type saddr_reg = SA_SP | SA_FP
  type saddr = saddr_reg * word

  type imm_or_saddr =
    | Imm   of word
    | SAddr of saddr

  (* ------------------------------------------------------------------ *)
  type 'lbl opcode_r =
    | OC_DCD   of word
    | OC_POP   of saddr option
    | OC_PUSH  of imm_or_saddr option
    | OC_CALL  of 'lbl
    | OC_RET
    | OC_LOAD  of imm_or_saddr
    | OC_STORE of saddr
    | OC_JMP   of 'lbl
    | OC_JMPZ  of 'lbl
    | OC_ALU   of alucode
    | OC_PRT
    | OC_STOP  

  and alucode =
    | ALU_Add  | ALU_Sub | ALU_Mul  | ALU_Div | ALU_Neg
    | ALU_Lt   | ALU_Gt  | ALU_Le   | ALU_Ge
    | ALU_LAnd | ALU_LOr | ALU_LNot
    | ALU_BAnd | ALU_BOr | ALU_BNot | ALU_BXor
  
  type opcode = word opcode_r

  (* ------------------------------------------------------------------ *)
  let reg_of_addr_reg = function
    | SA_FP -> `FP
    | SA_SP -> `SP

  (* ------------------------------------------------------------------ *)
  type vmerror =
    | InvalidAddress
    | DivisionByZero
    | InvalidIP
  
  exception VmError of vmerror
  exception VmStop

  (* ------------------------------------------------------------------ *)
  let string_of_vmerror = function
    | InvalidAddress -> "invalid stack address"
    | DivisionByZero -> "division by zero"
    | InvalidIP      -> "invalid instruction pointer"
  
  (* ------------------------------------------------------------------ *)
  module E = struct
    let invalid_address () =
      raise (VmError InvalidAddress)
  
    let division_by_zero () =
      raise (VmError DivisionByZero)

    let invalid_ip () =
      raise (VmError InvalidIP)
  end

  (* ------------------------------------------------------------------ *)
  module VMStack : sig
    type stack
  
    val create : int -> stack
    val capacity : stack -> int64
    val get : stack -> int64 -> word
    val set : stack -> int64 -> word -> unit
  end = struct
    type stack = word array
  
    let create (n : int) =
      Array.make n Int64.zero
  
    let capacity (st : stack) =
      Int64.of_int (Array.length st)
  
    let get (st : stack) (i : Int64.t) =
      if i < 0L || i >= capacity st then
        E.invalid_address ();
      Array.unsafe_get st (Int64.to_int i)
  
    let set (st : stack) (i : Int64.t) (w : word) =
      if i < 0L || i >= capacity st then
        E.invalid_address();
      Array.unsafe_set st (Int64.to_int i) w
  end
  
  (* ------------------------------------------------------------------ *)
  type register = FP | SP | IP | R0

  module VMRegisters : sig
    type registers

    val init : unit -> registers
    val get  : registers -> register -> word
    val set  : registers -> register -> word -> unit
  end = struct
    type registers = {
      mutable r_sp : word;
      mutable r_fp : word;
      mutable r_ip : word;
      mutable r_r0 : word;
    }

    let init () = { r_fp = 0L; r_sp = 0L; r_ip = 0L; r_r0 = 0L; }

    let get regs = function
      | FP -> regs.r_fp
      | SP -> regs.r_sp
      | IP -> regs.r_ip
      | R0 -> regs.r_r0
  
    let set regs reg w =
      match reg with
      | FP -> regs.r_fp <- w
      | SP -> regs.r_sp <- w
      | IP -> regs.r_ip <- w
      | R0 -> regs.r_r0 <- w
  end

  (* ------------------------------------------------------------------ *)
  module ALU : sig
    val op_add   : word -> word -> word
    val op_sub   : word -> word -> word
    val op_mul   : word -> word -> word
    val op_div   : word -> word -> word
    val op_neg   : word -> word
    val op_lt    : word -> word -> word
    val op_gt    : word -> word -> word
    val op_le    : word -> word -> word
    val op_ge    : word -> word -> word
    val op_land  : word -> word -> word
    val op_lor   : word -> word -> word
    val op_lnot  : word -> word
    val op_band  : word -> word -> word
    val op_bor   : word -> word -> word
    val op_bnot  : word -> word
    val op_bxor  : word -> word -> word
  end = struct
    let b2w (b : bool) =
      if b then 1L else 0L
  
    let op_add   = Int64.add
    let op_sub   = Int64.sub
    let op_mul   = Int64.mul
    let op_div   = fun x y ->
                     try  Int64.div x y
                     with Division_by_zero -> E.division_by_zero ()
    let op_neg   = Int64.neg
    let op_lt    = fun w1 w2 -> b2w (w1 <  w2)
    let op_gt    = fun w1 w2 -> b2w (w1 >  w2)
    let op_le    = fun w1 w2 -> b2w (w1 <= w2)
    let op_ge    = fun w1 w2 -> b2w (w1 >= w2)
    let op_land  = fun w1 w2 -> b2w (w1 <> 0L && w2 <> 0L)
    let op_lor   = fun w1 w2 -> b2w (w1 <> 0L || w2 <> 0L)
    let op_lnot  = fun w1    -> b2w (w1 = 0L)
    let op_band  = Int64.logand
    let op_bor   = Int64.logor
    let op_bnot  = Int64.lognot
    let op_bxor  = Int64.logxor
  end

  (* ------------------------------------------------------------------ *)
  type vm = {
    vm_stack : VMStack.stack;
    vm_regs  : VMRegisters.registers;
    vm_code  : opcode array;
    vm_out   : Format.formatter;
  }
    
  (* ------------------------------------------------------------------ *)
  let sp (vm : vm) = VMRegisters.get vm.vm_regs SP
  let fp (vm : vm) = VMRegisters.get vm.vm_regs FP
  let ip (vm : vm) = VMRegisters.get vm.vm_regs IP
  let r0 (vm : vm) = VMRegisters.get vm.vm_regs R0

  (* ------------------------------------------------------------------ *)
  let w_sp (vm : vm) (w : word) = VMRegisters.set vm.vm_regs SP w
  let w_fp (vm : vm) (w : word) = VMRegisters.set vm.vm_regs FP w
  let w_ip (vm : vm) (w : word) = VMRegisters.set vm.vm_regs IP w
  let w_r0 (vm : vm) (w : word) = VMRegisters.set vm.vm_regs R0 w
  
  (* ------------------------------------------------------------------ *)
  let create ?(out = stderr) code (stsz : int) : vm =
    let out = Format.formatter_of_out_channel out in

    { vm_stack = VMStack.create stsz;
      vm_regs  = VMRegisters.init ();
      vm_out   = out;
      vm_code  = code; }
  
  (* ------------------------------------------------------------------ *)
  let read (vm : vm) (addr : word) =
    VMStack.get vm.vm_stack addr
  
  (* ------------------------------------------------------------------ *)
  let write (vm : vm) (addr : word) (w : word) =
    VMStack.set vm.vm_stack addr w
  
  (* ------------------------------------------------------------------ *)
  let rd_reg (vm : vm) = function
    | `FP -> fp vm
    | `SP -> sp vm
    | `IP -> ip vm
    | `R0 -> r0 vm
  
  (* ------------------------------------------------------------------ *)
  let vm_push (vm : vm) (w : word) =
    write vm (sp vm) w; w_sp vm (Int64.add (sp vm) 1L)
    
  (* ------------------------------------------------------------------ *)
  let vm_pop (vm : vm) =
    w_sp vm (Int64.sub (sp vm) 1L); read vm (sp vm)
  
  (* ------------------------------------------------------------------ *)
  let vm_dcd (vm : vm) (i : int64) =
    w_sp vm (Int64.sub (sp vm) i)

  (* ------------------------------------------------------------------ *)
  let fetch_r ~incr (vm : vm) =
    let ip = ip vm in
    if ip < 0L || ip >= Int64.of_int (Array.length vm.vm_code) then
      E.invalid_ip ();
    let oc = vm.vm_code.(Int64.to_int ip) in
    (if incr then w_ip vm (Int64.add ip 1L)); oc

  (* ------------------------------------------------------------------ *)
  let fetch = fetch_r ~incr:true

  (* ------------------------------------------------------------------ *)
  let resolve_saddr (vm : vm) (reg, off) =
    Int64.add (rd_reg vm (reg_of_addr_reg reg)) off

  (* ------------------------------------------------------------------ *)
  let exec_unioc (vm : vm) (op : word -> word) =
    let x = read vm (Int64.sub (sp vm) 1L) in
    write vm (Int64.sub (sp vm) 1L) (op x)
  
  (* ------------------------------------------------------------------ *)
  let exec_binoc (vm : vm) (op : word -> word -> word) =
    let x = read vm (Int64.sub (sp vm) 1L) in
    let y = read vm (Int64.sub (sp vm) 2L) in
    let z = op x y in
    write vm (Int64.sub (sp vm) 2L) z;
    w_sp  vm (Int64.sub (sp vm) 1L)
  
  (* ------------------------------------------------------------------ *)
  let exec1 (vm : vm) (oc : opcode) =
    match oc with
    | OC_PRT ->
        Format.fprintf vm.vm_out "%Ld\n%!" (r0 vm)
   
    | OC_DCD i ->
        vm_dcd vm i

    | OC_POP None ->
        w_r0 vm (vm_pop vm)
  
    | OC_POP Some addr ->
        let addr = resolve_saddr vm addr in
        write vm addr (vm_pop vm)

    | OC_PUSH None ->
        vm_push vm (r0 vm)
  
    | OC_PUSH (Some (Imm w)) ->
        vm_push vm w

    | OC_PUSH (Some (SAddr addr)) ->
        let addr = resolve_saddr vm addr in
        vm_push vm (read vm addr)

    | OC_CALL lbl ->
        vm_push vm (ip vm);
        vm_push vm (fp vm);
        w_fp vm (sp vm);
        w_ip vm lbl
  
    | OC_RET ->
        w_sp vm (fp vm);
        w_fp vm (vm_pop vm);
        w_ip vm (vm_pop vm);

    | OC_JMP target ->
        w_ip vm target
  
    | OC_JMPZ target ->
        if r0 vm = 0L then w_ip vm target
  
    | OC_LOAD (SAddr addr) ->
        let addr = resolve_saddr vm addr in
        w_r0 vm (read vm addr)
  
    | OC_LOAD (Imm w) ->
        w_r0 vm w
  
    | OC_STORE addr ->
        let addr = resolve_saddr vm addr in
        write vm addr (r0 vm)
  
    | OC_ALU ALU_Add  -> exec_binoc vm ALU.op_add
    | OC_ALU ALU_Sub  -> exec_binoc vm ALU.op_sub
    | OC_ALU ALU_Mul  -> exec_binoc vm ALU.op_mul
    | OC_ALU ALU_Div  -> exec_binoc vm ALU.op_div
    | OC_ALU ALU_Neg  -> exec_unioc vm ALU.op_neg
    | OC_ALU ALU_Lt   -> exec_binoc vm ALU.op_lt
    | OC_ALU ALU_Gt   -> exec_binoc vm ALU.op_gt
    | OC_ALU ALU_Le   -> exec_binoc vm ALU.op_le
    | OC_ALU ALU_Ge   -> exec_binoc vm ALU.op_ge
    | OC_ALU ALU_LAnd -> exec_binoc vm ALU.op_land
    | OC_ALU ALU_LOr  -> exec_binoc vm ALU.op_lor
    | OC_ALU ALU_LNot -> exec_unioc vm ALU.op_lnot
    | OC_ALU ALU_BAnd -> exec_binoc vm ALU.op_band
    | OC_ALU ALU_BOr  -> exec_binoc vm ALU.op_bor
    | OC_ALU ALU_BNot -> exec_unioc vm ALU.op_bnot
    | OC_ALU ALU_BXor -> exec_binoc vm ALU.op_bxor

    | OC_STOP -> raise VmStop

  (* ------------------------------------------------------------------ *)
  let exec ?(dump = (fun _ _ -> ())) (vm : vm) =
    let i = ref 0 in
    try
      while true do
        dump !i vm; incr i; exec1 vm (fetch vm)
      done
    with VmStop -> dump (-1) vm
end

(* ==================================================================== *)
module HiAsm : sig
  (* ------------------------------------------------------------------ *)
  exception InvalidSrcAsm

  (* ------------------------------------------------------------------ *)
  type lopcode = [
    | `Instr of sopcode
    | `Label of string
  ]

  and sopcode = string VM.opcode_r
  and srcasm  = lopcode list

  (* ------------------------------------------------------------------ *)
  val resolve : srcasm -> VM.opcode list
end = struct
  (* ------------------------------------------------------------------ *)
  exception InvalidSrcAsm

  (* ------------------------------------------------------------------ *)
  type lopcode = [
    | `Instr of sopcode
    | `Label of string
  ]

  and sopcode = string VM.opcode_r
  and srcasm  = lopcode list

  (* ------------------------------------------------------------------ *)
  let resolve (ocs : srcasm) : VM.opcode list =
    let labels =
      snd (List.fold_left (fun (i, map) -> function
        | `Instr _ -> (i+1, map)
        | `Label s ->
            if SMap.mem s map then raise InvalidSrcAsm;
            (i, SMap.add s (Int64.of_int i) map))
      (0, SMap.empty) ocs) in

    let r l =
      try  SMap.find l labels
      with Not_found -> raise InvalidSrcAsm in

    let resolve1 = function
      | VM.OC_DCD   i -> VM.OC_DCD  i
      | VM.OC_POP   a -> VM.OC_POP  a
      | VM.OC_PUSH  a -> VM.OC_PUSH a
      | VM.OC_CALL  l -> VM.OC_CALL (r l)
      | VM.OC_RET     -> VM.OC_RET
      | VM.OC_LOAD  a -> VM.OC_LOAD  a
      | VM.OC_STORE a -> VM.OC_STORE a
      | VM.OC_JMP   l -> VM.OC_JMP   (r l)
      | VM.OC_JMPZ  l -> VM.OC_JMPZ  (r l)
      | VM.OC_ALU   c -> VM.OC_ALU   c
      | VM.OC_PRT     -> VM.OC_PRT
      | VM.OC_STOP    -> VM.OC_STOP
    in

    List.rev (List.fold_left (fun ocs -> function
      | `Label _ -> ocs
      | `Instr i -> resolve1 i :: ocs) [] ocs)
end

(* ==================================================================== *)
module PP = struct
  open VM

  (* ------------------------------------------------------------------ *)
  let string_of_alucode (alu : alucode) =
    match alu with
    | ALU_Add  -> "ADD"
    | ALU_Sub  -> "SUB"
    | ALU_Mul  -> "MUL"
    | ALU_Div  -> "DIV"
    | ALU_Neg  -> "NEG"
    | ALU_Lt   -> "LT"
    | ALU_Gt   -> "GT"
    | ALU_Le   -> "LE"
    | ALU_Ge   -> "GE"
    | ALU_LAnd -> "LAND"
    | ALU_LOr  -> "LOR"
    | ALU_LNot -> "LNOT"
    | ALU_BAnd -> "BAND"
    | ALU_BOr  -> "BOR"
    | ALU_BNot -> "BNOT"
    | ALU_BXor -> "BXOR"

  (* ------------------------------------------------------------------ *)
  let string_of_register = function
    | `FP -> "FP"
    | `SP -> "SP"
    | `IP -> "IP"
    | `R0 -> "R0"

  (* ------------------------------------------------------------------ *)
  let string_of_saddr ((reg, off) : saddr) =
    let r = "%" ^ (string_of_register (reg_of_addr_reg reg)) in
    let o = Int64.abs off in
    let s = if off < 0L then '-' else '+' in
    Printf.sprintf "%s%c%Ld" r s o

  (* ------------------------------------------------------------------ *)
  let mnemonic_of_opcode (oc : 'lbl opcode_r) =
    match oc with
    | OC_DCD   i                  -> ("DCD"  , [`Dec i])
    | OC_POP   None               -> ("POP"  , [])
    | OC_POP   (Some a)           -> ("POP"  , [`SAddr a])
    | OC_PUSH  None               -> ("PUSH" , [])
    | OC_PUSH  (Some (Imm w))     -> ("PUSH" , [`Dec w])
    | OC_PUSH  (Some (SAddr a))   -> ("PUSH" , [`SAddr a])
    | OC_CALL  lbl                -> ("CALL" , [`Lbl lbl])
    | OC_RET                      -> ("RET"  , [])
    | OC_LOAD  (Imm w)            -> ("LOAD" , [`Dec w])
    | OC_LOAD  (SAddr a)          -> ("LOAD" , [`SAddr a])
    | OC_STORE a                  -> ("STORE", [`SAddr a])
    | OC_JMP   lbl                -> ("JMP"  , [`Lbl lbl])
    | OC_JMPZ  lbl                -> ("JMPZ" , [`Lbl lbl])
    | OC_ALU   alu                -> ("ALU"  , [`Alu alu])
    | OC_PRT                      -> ("PRT"  , [])
    | OC_STOP                     -> ("STOP" , [])

  (* ------------------------------------------------------------------ *)
  let string_of_mnemonic_arg pplbl = function
    | `Dec   i -> Printf.sprintf "0x%.2Lx" i
    | `SAddr a -> string_of_saddr a
    | `Lbl   l -> pplbl l
    | `Alu   a -> string_of_alucode a

  (* ------------------------------------------------------------------ *)
  let string_of_opcode pplbl (oc : 'lbl opcode_r) =
    let m, args = mnemonic_of_opcode oc in
    let args = List.map (string_of_mnemonic_arg pplbl) args in

    if   args = []
    then m
    else Printf.sprintf "%-.*s\t%s" 5 m (String.concat ", " args)
 
  (* ------------------------------------------------------------------ *)
  let string_of_srcasm1 pplbl = function
    | `Instr oc  -> Printf.sprintf "\t%s" (string_of_opcode pplbl oc)
    | `Label lbl -> Printf.sprintf ".%s:" (pplbl lbl)

  (* ------------------------------------------------------------------ *)
  let pp_opcodes fmt srcasm =
    let do1 = string_of_opcode (Printf.sprintf "0x%.2Lx") in
    List.iter (fun x -> Format.fprintf fmt "%s\n%!" (do1 x)) srcasm

  (* ------------------------------------------------------------------ *)
  let pp_srcasm fmt srcasm =
    let do1 = string_of_srcasm1 (fun x -> x) in
    List.iter (fun x -> Format.fprintf fmt "%s\n%!" (do1 x)) srcasm
end

(* ==================================================================== *)
module Dump : sig
  open VM

  val regs  : Format.formatter -> VMRegisters.registers -> unit
  val stack : ?bound:int -> Format.formatter -> VMStack.stack -> unit
  val vm    : Format.formatter -> VM.vm -> unit
end = struct
  open VM

  let pp_word ?(prefix = false) fmt (w : word) =
    Format.fprintf fmt "%s%.16Lx" (if prefix then "0x" else "") w

  let regs fmt (regs : VMRegisters.registers) =
    Format.fprintf fmt "[SP: %a] [FP: %a]\n%!"
      (pp_word ~prefix:true) (VMRegisters.get regs SP)
      (pp_word ~prefix:true) (VMRegisters.get regs FP);
    Format.fprintf fmt "[IP: %a] [R0: %a]\n%!"
      (pp_word ~prefix:true) (VMRegisters.get regs IP)
      (pp_word ~prefix:true) (VMRegisters.get regs R0)

  let stack ?(bound = max_int) fmt (stack : VMStack.stack) =
    let fixnl i : ('a, 'b, 'c, 'd, 'd, 'a) format6 =
      match i, i mod 4 with
      | 0, _ -> ""
      | _, 0 -> "\n%!"
      | _, 1 -> " "
      | _, 2 -> "  "
      | _, 3 -> " "
      | _, _ -> assert false
    in

    let cpt, wd = (Int64.to_int (VMStack.capacity stack), 4) in

    for i = 0 to (min bound cpt) - 1 do
      let w = VMStack.get stack (Int64.of_int i) in
      Format.fprintf fmt "%(%)%s%a"
        (fixnl i)
        (if i mod 4 = 0 then Printf.sprintf "0x%0.*x " wd i else "")
        (pp_word ~prefix:false)  w
    done;
    if bound < cpt then
      Format.fprintf fmt "%(%)..." (fixnl bound);
    Format.fprintf fmt "\n%!"

  let vm fmt (vm : vm) =
    let extra = 4L in
    let bound =
      if   VM.sp vm < Int64.sub (VMStack.capacity vm.vm_stack) extra
      then Int64.add (VM.sp vm) extra
      else VMStack.capacity vm.vm_stack in
    let bound = Int64.to_int bound in

    let ip =
      try  Some (VM.fetch_r ~incr:false vm)
      with VM.VmError _ -> None in
  
    let ppoc = PP.string_of_opcode (Printf.sprintf "0x%.2Lx") in

    Format.fprintf fmt "REGS:\n%a\n%!" regs vm.vm_regs;
    Format.fprintf fmt "OC:\n\t%s\n%!" (odfl "<invalid>" (omap ppoc ip));
    Format.fprintf fmt "STACK:\n%a%!" (stack ~bound) vm.vm_stack
end

(* ==================================================================== *)
module Parser = struct
  open VM

  (* ------------------------------------------------------------------ *)
  let of_syntax =
    let module A = Syntax.Asm in
    let module L = Syntax.Location in

    let rec tt_alucode = function
      | `ADD  -> ALU_Add
      | `SUB  -> ALU_Sub
      | `MUL  -> ALU_Mul
      | `DIV  -> ALU_Div
      | `LE   -> ALU_Le
      | `LT   -> ALU_Lt
      | `LAND -> ALU_LAnd
      | `LOR  -> ALU_LOr
      | `LNOT -> ALU_LNot
      | `NEG  -> ALU_Neg

    and tt_addr = function A.P_ALbl x -> x

    and tt_saddr (r, o) =
      match r with
      | `FP -> (SA_FP, o)
      | `SP -> (SA_SP, o)

    and tt_imm_or_saddr = function
      | `Imm   x -> Imm x
      | `Stack x -> SAddr (tt_saddr x)

    and tt_instr (i : A.pinstr) : string opcode_r =
      match L.unloc i with
      | A.P_DCD   x -> OC_DCD x
      | A.P_POP   x -> OC_POP (omap tt_saddr x)
      | A.P_PUSH  x -> OC_PUSH (omap tt_imm_or_saddr x)
      | A.P_CALL  x -> OC_CALL (tt_addr x)
      | A.P_RET     -> OC_RET
      | A.P_LOAD  x -> OC_LOAD (tt_imm_or_saddr x)
      | A.P_STORE x -> OC_STORE (tt_saddr x)
      | A.P_JMP   x -> OC_JMP (tt_addr x)
      | A.P_JMPZ  x -> OC_JMPZ (tt_addr x)
      | A.P_ALU   x -> OC_ALU (tt_alucode x :> alucode)
      | A.P_PRT     -> OC_PRT
      | A.P_STOP    -> OC_STOP in

    let do1 = function
      | A.P_Empty   -> None
      | A.P_Label s -> Some (`Label (L.unloc s))
      | A.P_Instr i -> Some (`Instr (tt_instr i)) in

    fun ast ->
      List.rev (List.fold_left (fun acc x ->
        ofold List.cons acc (do1 x)) [] ast)
end
