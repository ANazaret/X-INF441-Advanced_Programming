type word = Int64.t

module VMRegister : sig
	type register = SP | FP | IP | R0
	type registers
	val create : unit -> registers
	val set : registers -> register -> word -> unit
	val get :registers -> register -> word
	val incr :registers -> register -> word -> unit
	
	end = struct
		type register = SP | FP | IP | R0
		type registers = { mutable sp : word; mutable fp : word; mutable ip : word; mutable r0 : word}
		let create() = {sp = Int64.zero; fp = Int64.zero; ip = Int64.zero; r0 = Int64.zero}
		let set regs reg w = match reg with
			| SP -> regs.sp <- w
			| FP -> regs.fp <- w
			| IP -> regs.ip <- w
			| R0 -> regs.r0 <- w
		let get regs reg = match reg with
			| SP -> regs.sp
			| FP -> regs.fp
			| IP -> regs.ip
			| R0 -> regs.r0
		let incr regs reg w = match reg with
			| SP -> regs.sp <- Int64.add w regs.sp
			| FP -> regs.fp <- Int64.add w regs.fp
			| IP -> regs.ip <- Int64.add w regs.ip
			| R0 -> regs.r0 <- Int64.add w regs.r0
	end

type alucode = 
	| ADD
	| MUL
	| LT
	| AND
	| NOT
	| SUB
	| DIV
	| LE
	| OR
	| NEG

type address = VMRegister.register * word

type opcode =
	| PRT
	| DCD of word
	| POP of (address option)
	| PUSH of (word option)
	| LOAD_w of word
	| LOAD_add of address
	| STORE of address
	| JMP of address
	| JMPZ of address
	| ALU of alucode
	| CALL of address
	| RET
	| STOP

type vmerror =
	| InvalidAddress
	| DivisionByZero

exception VMError of vmerror

module VMStack : sig
	type stack

 	val create : int -> stack
  val capacity : stack -> int
  val get : stack -> int64 -> word
  val set : stack -> int64 -> word -> unit
	
end = struct
  type stack = word array
	let create size = Array.make size 0L
	let capacity st = Array.length st
	let check_address st idx =
		let i = Int64.to_int idx in
		if i < 0 || i >= (Array.length st) then raise (VMError InvalidAddress)
	let get st idx = check_address st idx; Array.get st (Int64.to_int idx)
	let set st idx w = check_address st idx; Array.set st (Int64.to_int idx) w
	
end

module ALU : sig
  val op_add   : word -> word -> word
  val op_sub   : word -> word -> word
  val op_mul   : word -> word -> word
  val op_div   : word -> word -> word
  val op_neg   : word -> word
  val op_lt    : word -> word -> word
  val op_le    : word -> word -> word
  val op_land  : word -> word -> word
  val op_lor   : word -> word -> word
  val op_lnot  : word -> word
end = struct
  let b2w (b : bool) = if b then 1L else 0L

  let op_add = Int64.add
  let op_sub = Int64.sub
	let op_mul = Int64.mul
	let op_div a b = if b = 0L then raise (VMError DivisionByZero) else Int64.div a b
	let op_neg = Int64.neg
	let op_lt a b = b2w (a < b)
	let op_le a b = b2w (a <= b)
	let op_land = Int64.logand
	let op_lor = Int64.logor
	let op_lnot = Int64.lognot
end

type vm = {
	reg : VMRegister.registers; 
	stack : VMStack.stack; 
	opcode : opcode array
}


exception VMStop

let fetch (vm : vm) : opcode = 
	let ip = VMRegister.get vm.reg VMRegister.IP in
	VMRegister.incr vm.reg VMRegister.IP 1L;
	vm.opcode.(Int64.to_int ip)

(* Execute on [vm] the opcode [oc]. Raise [VMStop] when of the [vm]
 * wants to halt. *)

let exec1 (vm : vm) (oc : opcode) : unit = 
	match oc with
	| PRT -> print_int (Int64.to_int (VMRegister.get vm.reg VMRegister.R0))
	| DCD(imm) -> VMRegister.incr vm.reg VMRegister.SP (Int64.neg imm)
	| POP(None) -> let indexhd = VMRegister.get vm.reg VMRegister.SP in 
		let res = VMStack.get vm.stack indexhd in 
		VMRegister.incr vm.reg VMRegister.SP (-1L);
		VMRegister.set vm.reg VMRegister.R0 res;
		
	
			
			