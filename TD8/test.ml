
open Asmvm
open Asmvm.VM
open Asmvm.HiAsm

open Vmlib
open Vmlib.Ast

module SMap = Map.Make(String)

type cglocals = int SMap.t

(*
val cg_expr : cglocals -> Ast.expr -> srcasm
*)

let rec cg_expr cgloc exp : srcasm = match exp with
  | EVar(ident, type_) -> [`Instr(OC_LOAD( SAddr(SA_FP,Int64.of_int (SMap.find ident cgloc)))); `Instr(OC_PUSH(None))]
	| EBool(b) -> [`Instr(OC_PUSH(Some( Imm(Int64.one ))))]
	| EInt(x) -> [`Instr(OC_PUSH(Some( Imm(x))))]
	| EUni(U_Not, expr) -> (cg_expr cgloc expr)@[`Instr(OC_ALU(ALU_LNot))]
	| EUni(U_Neg, expr) -> (cg_expr cgloc expr)@[`Instr(OC_ALU(ALU_Neg))]
	| EBin(binop, (e1, e2) ) -> let alu = match binop with 
    		| B_And -> ALU_LAnd
    		| B_Or  -> ALU_LOr
        | B_Add -> ALU_Add
    		| B_Sub -> ALU_Sub
    		| B_Mul -> ALU_Mul
    		| B_Div -> ALU_Div
        | B_Lt  -> ALU_Lt
    		| B_Gt  -> ALU_Gt
    		| B_Le  -> ALU_Le
    		| B_Ge  -> ALU_Ge
		in 
		(cg_expr cgloc e1)@(cg_expr cgloc e2)@[`Instr(OC_ALU(alu))]

	| _ -> failwith "not implemented"

(*
  | EBin  of binop * (expr * expr)
  | ECmp  of expr * expr
  | ECall of fname * expr list
*)

(* 
  val cg_instr : cglocals -> Ast.instr -> srcasm
	val cg_stmt  : cglocals -> Ast.stmt  -> srcasm
*)	

let rec cg_instr locs = function
	| IPrint(expr,type_) -> cg_expr locs expr @[`Instr(OC_POP(None)); `Instr(OC_PRT) ]
  | IAssign(None,expr) -> cg_expr locs expr@[`Instr(OC_POP(None))]
  | IAssign(Some(ident, type_), expr) -> cg_expr locs expr@[`Instr(OC_POP(None)); `Instr(OC_STORE(None)); SA_FP,Int64.of_int (SMap.find ident cgloc)]
  | IIf(expr, st1, st2) -> 
  | IWhile  of expr * stmt
  | IReturn of (expr * type_ option) option

and cg_stmt locs = function
	| 
