(* -------------------------------------------------------------------- *)
module Location : sig
  (* ------------------------------------------------------------------ *)
  open Lexing

  (* ------------------------------------------------------------------ *)
  type t = {
    loc_fname : string;
    loc_start : int * int;
    loc_end   : int * int;
    loc_bchar : int;
    loc_echar : int;
  }

  (* ------------------------------------------------------------------ *)
  val _dummy    : t
  val make      : position -> position -> t
  val of_lexbuf : lexbuf -> t

  val merge    : t -> t -> t
  val mergeall : t list -> t

  val isdummy : t -> bool

  val tostring : t -> string

  (* ------------------------------------------------------------------ *)
  type 'a loced = { plloc : t; pldesc : 'a; }

  val mkloc  : t -> 'a -> 'a loced
  val loc    : 'a loced -> t
  val unloc  : 'a loced -> 'a
  val unlocs : ('a loced) list -> 'a list
  val lmap   : ('a -> 'b) -> 'a loced -> 'b loced
end = struct
  (* ------------------------------------------------------------------ *)
  open Lexing

  (* ------------------------------------------------------------------ *)
  type t = {
    loc_fname : string;
    loc_start : int * int;
    loc_end   : int * int;
    loc_bchar : int;
    loc_echar : int;
  }

  let _dummy : t = {
    loc_fname = "";
    loc_start = (-1, -1);
    loc_end   = (-1, -1);
    loc_bchar = -1;
    loc_echar = -1;
  }

  (* ------------------------------------------------------------------ *)
  let make (p1 : position) (p2 : position) =
    let mkpos (p : position) =
      (p.pos_lnum, p.pos_cnum - p.pos_bol)
    in
      { loc_fname = p1.pos_fname;
        loc_start = mkpos p1    ;
        loc_end   = mkpos p2    ;
        loc_bchar = p1.pos_cnum ;
        loc_echar = p2.pos_cnum ; }

  let of_lexbuf (lb : lexbuf) =
    let p1 = Lexing.lexeme_start_p lb in
    let p2 = Lexing.lexeme_end_p lb in
    make p1 p2

  (* --------------------------------------------------------------------- *)
  let merge (p1 : t) (p2 : t) =
    { loc_fname = p1.loc_fname;
      loc_start = min p1.loc_start p2.loc_start;
      loc_end   = max p1.loc_end   p2.loc_end  ;
      loc_bchar = min p1.loc_bchar p2.loc_bchar;
      loc_echar = max p1.loc_echar p2.loc_echar; }

  let mergeall (p : t list) =
    match p with
    | []      -> _dummy
    | t :: ts -> List.fold_left merge t ts

  let isdummy (p : t) =
    p.loc_bchar < 0 || p.loc_echar < 0

  (* ------------------------------------------------------------------ *)
  let tostring (p : t) =
    let spos =
      if p.loc_start = p.loc_end then
        Printf.sprintf "line %d (%d)"
          (fst p.loc_start) (snd p.loc_start)
      else if fst p.loc_start = fst p.loc_end then
        Printf.sprintf "line %d (%d-%d)"
          (fst p.loc_start) (snd p.loc_start + 1) (snd p.loc_end + 1)
      else
        Printf.sprintf "line %d (%d) to line %d (%d)"
          (fst p.loc_start) (snd p.loc_start + 1)
          (fst p.loc_end  ) (snd p.loc_end   + 1)
    in
      Printf.sprintf "%s: %s" p.loc_fname spos

  (* ------------------------------------------------------------------ *)
  type 'a loced = { plloc : t; pldesc : 'a; }

  (* ------------------------------------------------------------------ *)
  let loc    x = x.plloc
  let unloc  x = x.pldesc
  let unlocs x = List.map unloc x

  let lmap (f : 'a -> 'b) (x : 'a loced) =
    { x with pldesc = f x.pldesc }

  let mkloc (loc : t) (x : 'a) : 'a loced =
    { plloc = loc; pldesc = x; }
end

(* -------------------------------------------------------------------- *)
module L = Location

(* -------------------------------------------------------------------- *)
exception ParseError of (Location.t * string option)

let sparse_error ?msg loc =
  raise (ParseError (loc, msg))

(* -------------------------------------------------------------------- *)
module Asm = struct
  (* ------------------------------------------------------------------ *)
  type palu = [
    | `ADD | `SUB | `MUL | `DIV 
    | `NEG | `LE  | `LT  | `LAND
    | `LOR | `LNOT
  ]
  
  type psaddr = [`SP | `FP] * int64

  type pinstr_r =
    | P_DCD   of int64
    | P_POP   of psaddr option
    | P_PUSH  of [`Imm of int64 | `Stack of psaddr] option
    | P_CALL  of paddr
    | P_RET
    | P_LOAD  of [`Imm of int64 | `Stack of psaddr]
    | P_STORE of ([`SP | `FP] * int64)
    | P_JMP   of paddr
    | P_JMPZ  of paddr
    | P_ALU   of palu
    | P_PRT
    | P_STOP  
  
  and pentry =
    | P_Empty
    | P_Instr of pinstr
    | P_Label of plabel
  
  and paddr =
    | P_ALbl of string
  
  and plabel   = string L.loced
  and pinstr   = pinstr_r L.loced
  and pprogram = pentry list
  
end

(* -------------------------------------------------------------------- *)
type symbol  = string
type psymbol = symbol L.loced

(* -------------------------------------------------------------------- *)
type ptype_r = PTBool | PTInt

and ptype  = ptype_r L.loced
and prtype = ptype option

(* -------------------------------------------------------------------- *)
type puniop =
  | U_Not | U_Neg

type pbinop =
  | B_And | B_Or
  | B_Add | B_Sub | B_Mul | B_Div
  | B_Lt  | B_Gt  | B_Le  | B_Ge

(* -------------------------------------------------------------------- *)
type pexpr_r =
  | PEBool  of bool
  | PEInt   of Int64.t
  | PEVar   of psymbol
  | PEUni   of puniop * pexpr
  | PEBin   of pbinop * (pexpr * pexpr)
  | PEEq    of bool * (pexpr * pexpr)
  | PECall  of psymbol * pexpr list

and pexpr = pexpr_r L.loced

(* -------------------------------------------------------------------- *)
type lvalue = psymbol

type pinstr_r =
  | PIEmpty
  | PIPrint  of pexpr
  | PIAssign of lvalue option * pexpr
  | PIIf     of pexpr * (pstmt * pstmt option)
  | PIWhile  of pexpr * pstmt
  | PIReturn of pexpr option

and pstmt_r = pinstr list

and pinstr = pinstr_r L.loced
and pstmt  = pstmt_r  L.loced

(* -------------------------------------------------------------------- *)
type pproc_r = {
  pr_name   : psymbol;
  pr_retty  : prtype;
  pr_args   : (ptype * psymbol) list;
  pr_locals : (ptype * (psymbol * pexpr option) list) list;
  pr_body   : pstmt;
}

type pproc = pproc_r L.loced

(* -------------------------------------------------------------------- *)
type pprogtop = TProc of pproc
type pprogram = pprogtop list
