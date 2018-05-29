(* -------------------------------------------------------------------- *)
module Ast : sig
  (* ------------------------------------------------------------------ *)
  type ident = string           (* variables  names *)
  type fname = string           (* procedures names *)
  
  (* ------------------------------------------------------------------ *)
  type type_ = TBool | TInt     (* bool / int types *)
  
  type uniop =
    | U_Not                     (* logical negation *)
    | U_Neg                     (* integer negation *)
  
  type binop =
    | B_And                     (* logical and                     *)
    | B_Or                      (* logical or                      *)
    | B_Add                     (* signed integer addition         *)
    | B_Sub                     (* signed integer substraction     *)
    | B_Mul                     (* signed integer multiplication   *)
    | B_Div                     (* signed integer division         *)
    | B_Lt                      (* signed integer comparisons (<)  *)
    | B_Gt                      (* signed integer comparisons (>)  *)
    | B_Le                      (* signed integer comparisons (<=) *)
    | B_Ge                      (* signed integer comparisons (>=) *)

  (* Typed AST for expressions. In order:       *)
  (* - local variable (include proc. arguments) *)
  (* - boolean and integer literal              *)
  (* - unary operators application              *)
  (* - binary operators application             *)
  (* - polymorphic equality                     *)
  (* - procedure call                           *)
  type expr =
    | EVar  of ident * type_                   (* local variable    *)
    | EBool of bool                            (* boolean literal   *)
    | EInt  of Int64.t                         (* integer literal   *)
    | EUni  of uniop * expr                    (* unary operator    *)
    | EBin  of binop * (expr * expr)           (* binary operator   *)
    | ECmp  of expr * expr                     (* signed comparison *)
    | ECall of fname * expr list               (* procedure call    *)
  
  type instr =
    | IPrint  of (expr * type_)                (* printing           *)
    | IAssign of (ident * type_) option * expr (* assignment         *)
    | IIf     of expr * (stmt * stmt)          (* conditional        *)
    | IWhile  of expr * stmt                   (* while loop         *)
    | IReturn of (expr * type_ option) option  (* return instruction *)
  
  and stmt = instr list
  
  (* ------------------------------------------------------------------ *)
  type procdef = {
    p_name  : fname;                (* name of the procedure *)
    p_args  : (ident * type_) list; (* procedure arguments   *)
    p_lcls  : (ident * type_) list; (* locals declarations   *)
    p_retty : type_ option;         (* optional return type  *)
    p_body  : stmt;                 (* body of the procedure *)
  }
end

(* -------------------------------------------------------------------- *)
type hierror                    (* parsing / typechecking errors *)

exception HiError of hierror    (* parsing / typechecking errors *)

(* -------------------------------------------------------------------- *)
(* [string_of_hierror exn] returns a string representation of [exn]     *)
val string_of_hierror : hierror -> string

(* -------------------------------------------------------------------- *)
(* [pexpr_of_string contents] parses the string contents, returning     *
 * the AST of the parsed expression. Raises a [HiError] on error.       *)
val pexpr_of_string : string -> Syntax.pexpr

(* -------------------------------------------------------------------- *)
(* [pstmt_of_string contents] parses the string contents, returning     *
 * the AST of the parsed instruction block. Raises a [HiError] on       *
 * error.                                                               *)
val pstmt_of_string : string -> Syntax.pstmt

(* -------------------------------------------------------------------- *)
(* [program_of_string contents] parses the string contents, returning   *
 * the AST of the parsed program. Raises a [HiError] on error.          *)
val pprogram_of_string : string -> Syntax.pprogram

(* -------------------------------------------------------------------- *)
(* [program_of_string filename] parses the contents of file [filename], *
 * returning the AST of the parsed program. Raise a [HiError] on error. *)
val pprogram_of_file   : string -> Syntax.pprogram

(* -------------------------------------------------------------------- *)
(* [tt_expr map ast] typechecks the given expression AST, returning     *
 * the checked expression along with its type. The argument [map]       *
 * contains the local variables bindings. Raises a [HiError] onerror.   *)
val tt_expr :
   (Ast.ident * Ast.type_) list -> Syntax.pexpr
   -> Ast.expr * (Ast.type_ option)

(* -------------------------------------------------------------------- *)
(* [tt_stmt map ast] typechecks the given statement AST, returning      *
 * the checked instruction block. The argument [map] contains the local *
 * variables bindings. Raises a [HiError] onerror.                      *)
val tt_stmt : (Ast.ident * Ast.type_) list -> Syntax.pstmt -> Ast.stmt

(* -------------------------------------------------------------------- *)
(* [tt_program ast] typechecks the given AST, returning the list of     *
 * procedure definitions. Raises a [HiError] on error.                  *)
val tt_program : Syntax.pprogram -> Ast.procdef list

(* -------------------------------------------------------------------- *)
(* [parse_and_tt_file_or_die filename] parses and typechecks the        *
 * contents of [filename] or die (with exit status 1) if a parse error  *
 * or typing error occured, printing the relevant error message of the  *
 * standard error.                                                      *)
val parse_and_tt_file_or_die : string -> Ast.procdef list

(* ==================================================================== *)
val pasm_of_string : string -> Syntax.Asm.pprogram
val pasm_of_file   : string -> Syntax.Asm.pprogram
