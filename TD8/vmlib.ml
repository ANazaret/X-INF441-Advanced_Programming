(* -------------------------------------------------------------------- *)
module P = Parser
module S = Syntax
module L = Syntax.Location

module SSet = Set.Make(String)
module SMap = Map.Make(String)

(* -------------------------------------------------------------------- *)
module Syntax = Syntax

(* -------------------------------------------------------------------- *)
module List = struct
  include List

  let map_fold f x0 s =
    let x, acc =
      List.fold_left (fun (x, acc) v ->
        let x, hd = f x v in (x, hd :: acc))
        (x0, []) s
    in (x, List.rev acc)
end

(* -------------------------------------------------------------------- *)
module String = struct
  include String

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let split ?(from = 0) str sep =
    let p    = index_from str from sep in
    let slen = length str in
    sub str 0 p, sub str (p + 1) (slen - p - 1)

  let trim (s : string) =
    let len = String.length s in
    let i, j = ref 0, ref (len-1) in

    while !i < len && is_space s.[!i] do incr i done;
    while !i < !j  && is_space s.[!j] do decr j done;

    let slen = !j - !i + 1 in

    if slen < len then String.sub s !i slen else s
end

(* -------------------------------------------------------------------- *)
let omap  f = function None -> None | Some x -> Some (f x)
let obind f = function None -> None | Some x -> f x

(* -------------------------------------------------------------------- *)
module Disposable : sig
  type 'a t

  exception Disposed

  val create  : ?cb:('a -> unit) -> 'a -> 'a t
  val get     : 'a t -> 'a
  val dispose : 'a t -> unit
end = struct
  type 'a t = ((('a -> unit) option * 'a) option) ref

  exception Disposed

  let get (p : 'a t) =
    match !p with
    | None -> raise Disposed
    | Some (_, x) -> x

  let dispose (p : 'a t) =
    let do_dispose p =
      match p with
      | Some (Some cb, x) -> cb x
      | _ -> ()
    in

    let oldp = !p in
      p := None; do_dispose oldp

  let create ?(cb : ('a -> unit) option) (x : 'a) =
    let r = ref (Some (cb, x)) in
    Gc.finalise (fun r -> dispose r) r; r
end

(* -------------------------------------------------------------------- *)
module Io : sig
  (* ------------------------------------------------------------------ *)
  type reader

  (* ------------------------------------------------------------------ *)
  val from_channel : name:string -> in_channel -> reader
  val from_file    : string -> reader
  val from_string  : name:string -> string -> reader
  val finalize     : reader -> unit

  (* ------------------------------------------------------------------ *)
  val parse_expr    : reader -> Syntax.pexpr
  val parse_stmt    : reader -> Syntax.pstmt
  val parse_program : reader -> Syntax.pprogram

  (* ------------------------------------------------------------------ *)
  val parse_asm_of_string  : name:string -> string -> Syntax.Asm.pprogram
  val parse_asm_of_channel : name:string -> in_channel -> Syntax.Asm.pprogram
  val parse_asm_of_file    : string -> Syntax.Asm.pprogram
end = struct
  (* ------------------------------------------------------------------ *)
  let lexbuf_from_channel = fun name channel ->
    let lexbuf = Lexing.from_channel channel in
      lexbuf.Lexing.lex_curr_p <- {
          Lexing.pos_fname = name;
          Lexing.pos_lnum  = 1;
          Lexing.pos_bol   = 0;
          Lexing.pos_cnum  = 0
        };
      lexbuf

  (* ------------------------------------------------------------------ *)
  type reader = Lexing.lexbuf Disposable.t

  let lexbuf (reader : reader) =
    Disposable.get reader

  (* ------------------------------------------------------------------ *)
  let from_channel ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    Disposable.create lexbuf

  (* ------------------------------------------------------------------ *)
  let from_file filename =
    let channel = open_in filename in

    try
      let lexbuf = lexbuf_from_channel filename channel in
      Disposable.create ~cb:(fun _ -> close_in channel) lexbuf

    with
      | e ->
          (try close_in channel with _ -> ());
          raise e

  (* ------------------------------------------------------------------ *)
  let from_string_x ?(lineno = 1) ~name data =
    let lexbuf = Lexing.from_string data in
    lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = name;
      Lexing.pos_lnum  = lineno;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0;
    };
    Disposable.create lexbuf

  (* ------------------------------------------------------------------ *)
  let from_string =
    from_string_x ~lineno:1

  (* ------------------------------------------------------------------ *)
  let finalize (reader : reader) =
    Disposable.dispose reader

  (* ------------------------------------------------------------------ *)
  let parse_expr (reader : reader) =
    Parser.xexpr Lexer.main (lexbuf reader)

  (* ------------------------------------------------------------------ *)
  let parse_stmt (reader : reader) =
    Parser.xstmt Lexer.main (lexbuf reader)

  (* ------------------------------------------------------------------ *)
  let parse_program (reader : reader) =
    Parser.xprogram Lexer.main (lexbuf reader)

  (* -------------------------------------------------------------------- *)
  let asm_parse_line ?lineno ~(name : string) (line : string) =
    let line   =
      try  fst (String.split line ';')
      with Not_found -> line in
  
    if String.trim line = "" then Syntax.Asm.P_Empty else
    let reader = from_string_x ?lineno ~name line in
    AsmParser.entry AsmLexer.main (lexbuf reader)

  (* -------------------------------------------------------------------- *)
  let parse_asm_of_channel ~(name : string) (inc : in_channel) =
    let rec aux (i, acc) =
      try
        let line = input_line inc in
        aux (i+1, asm_parse_line ~lineno:(i+1) ~name line :: acc)
      with End_of_file -> List.rev acc

    in aux (1, [])

  (* -------------------------------------------------------------------- *)
  let parse_asm_of_file (name : string) =
    let channel = open_in name in

    let ast =
      try
        parse_asm_of_channel ~name channel;
      with e ->
        (try close_in channel with _ -> ());
        raise e

    in close_in channel; ast

  (* -------------------------------------------------------------------- *)
  let parse_asm_of_string ~(name : string) (s : string) =
    let rec aux (i, acc) pos =
      if pos >= String.length s then List.rev acc else

      let end_ =
        try  String.index_from s pos '\n'
        with Not_found -> String.length s in

      let line = String.sub s pos (end_ - pos) in

      aux (i+1, asm_parse_line ~lineno:(i+1) ~name line :: acc) (end_+1)

    in aux (0, []) 0
end

(* -------------------------------------------------------------------- *)
module Ast = struct
  (* ------------------------------------------------------------------ *)
  type ident = string
  type fname = string

  (* ------------------------------------------------------------------ *)
  type type_ = TBool | TInt

  type uniop =
    | U_Not | U_Neg

  type binop =
    | B_And | B_Or
    | B_Add | B_Sub | B_Mul | B_Div
    | B_Lt  | B_Gt  | B_Le  | B_Ge

  type expr =
    | EVar  of ident * type_
    | EBool of bool
    | EInt  of Int64.t
    | EUni  of uniop * expr
    | EBin  of binop * (expr * expr)
    | ECmp  of expr * expr
    | ECall of fname * expr list

  type instr =
    | IPrint  of (expr * type_)
    | IAssign of (ident * type_) option * expr
    | IIf     of expr * (stmt * stmt)
    | IWhile  of expr * stmt
    | IReturn of (expr * type_ option) option

  and stmt = instr list

  (* ------------------------------------------------------------------ *)
  type procdef = {
    p_name  : fname;
    p_args  : (ident * type_) list;
    p_lcls  : (ident * type_) list;
    p_retty : type_ option;
    p_body  : stmt;
  }

  (* ------------------------------------------------------------------ *)
  let pp_type fmt (ty : type_) =
    match ty with
    | TBool -> Format.fprintf fmt "bool"
    | TInt  -> Format.fprintf fmt "int"
end

(* -------------------------------------------------------------------- *)
module Typing = struct
  (* ------------------------------------------------------------------ *)
  type tyerror =
    | UnknownVar         of S.symbol
    | UnknownFun         of S.symbol
    | InvalidArgsCount   of int * int
    | InvalidType        of Ast.type_ * Ast.type_ option
    | RetInVoid
    | PrintInVoid
    | DuplicatedProcName of S.symbol
    | DuplicatedVarName  of S.symbol

  exception TyError of (L.t * tyerror)

  let tyerror ~loc (code : tyerror) =
    TyError (loc, code)

  (* ------------------------------------------------------------------ *)
  let pp_tyerror fmt (code : tyerror) =
    match code with
    | UnknownVar x ->
        Format.fprintf fmt "unknown variable: `%s'" x

    | UnknownFun x ->
        Format.fprintf fmt "unknown procedure: `%s'" x

    | InvalidArgsCount (i, j) ->
        Format.fprintf fmt "expecting %d argument(s), got %d" i j

    | InvalidType (ty1, Some ty2) ->
        Format.fprintf fmt
          "expecting an expression of type @[%a@], got @[%a@]"
          Ast.pp_type ty1 Ast.pp_type ty2

    | InvalidType (ty, None) ->
        Format.fprintf fmt
          "expecting an expression of type @[%a@]"
          Ast.pp_type ty

    | RetInVoid ->
        Format.fprintf fmt "non-void return in void procedure"

    | PrintInVoid ->
        Format.fprintf fmt "printing of a void value requested"

    | DuplicatedProcName x ->
        Format.fprintf fmt "duplicated procedure name: `%s'" x

    | DuplicatedVarName x ->
        Format.fprintf fmt "duplicated variable name: `%s'" x

  (* ------------------------------------------------------------------ *)
  module Env : sig
    type env

    type var  = Ast.ident * Ast.type_
    type fun_ = Ast.ident * Ast.type_ list * Ast.type_ option

    val empty : env
    val of_bindings : var list -> env

    module Vars : sig
      val push   : env -> var -> env
      val find   : env -> S.symbol -> var option
      val exists : env -> S.symbol -> bool
    end

    module Funs : sig
      val push   : env -> fun_ -> env
      val find   : env -> S.symbol -> fun_ option
      val exists : env -> S.symbol -> bool
    end
  end = struct
    type var  = Ast.ident * Ast.type_
    type fun_ = Ast.ident * Ast.type_ list * Ast.type_ option

    type env = {
      e_vars : var  SMap.t;
      e_funs : fun_ SMap.t;
    }

    let empty = { e_vars = SMap.empty; e_funs = SMap.empty; }

    module Vars = struct
      let push (env : env) ((x, _) as v : var) =
        { env with e_vars = SMap.add x v env.e_vars }

      let find (env : env) (x : S.symbol) =
        try  Some (SMap.find x env.e_vars)
        with Not_found -> None

      let exists (env : env) (x : S.symbol) =
        SMap.mem x env.e_vars
    end

    module Funs = struct
      let push (env : env) ((x, _, _) as f : fun_) =
        { env with e_funs = SMap.add x f env.e_funs }

      let find (env : env) (x : S.symbol) =
        try  Some (SMap.find x env.e_funs)
        with Not_found -> None

      let exists (env : env) (x : S.symbol) =
        SMap.mem x env.e_funs
    end

    let of_bindings bindings =
      List.fold_left Vars.push empty bindings
  end

  (* ------------------------------------------------------------------ *)
  let tt_type (_ : Env.env) (pty : S.ptype) =
    match L.unloc pty with
    | S.PTBool -> Ast.TBool
    | S.PTInt  -> Ast.TInt

  (* ------------------------------------------------------------------ *)
  let tt_var (env : Env.env) ({ L.pldesc = x; L.plloc = loc }) =
    match Env.Vars.find env x with
    | None   -> raise (tyerror ~loc (UnknownVar x))
    | Some v -> v

  (* ------------------------------------------------------------------ *)
  let tt_fun (env : Env.env) ({ L.pldesc = f; L.plloc = loc }) =
    match Env.Funs.find env f with
    | None   -> raise (tyerror ~loc (UnknownFun f))
    | Some v -> v

  (* ------------------------------------------------------------------ *)
  let tt_ty_assign ~(to_ : Ast.type_) ~(from : L.t * Ast.type_ option) =
    if Some to_ <> snd from then
      raise (tyerror ~loc:(fst from) (InvalidType (to_, snd from)))

  (* ------------------------------------------------------------------ *)
  let tt_ty_assign_sig (sig_ : Ast.type_ list) ((loc, args) : 'a) =
    let nsig, nargs = List.length sig_, List.length args in
    if nsig <> nargs then
      raise (tyerror ~loc (InvalidArgsCount (nsig, nargs)));
    List.iter2 (fun to_ from -> tt_ty_assign ~to_ ~from) sig_ args

  (* ------------------------------------------------------------------ *)
  let uniop_of_puniop (o : S.puniop) : Ast.uniop =
    match o with
    | S.U_Neg -> Ast.U_Neg
    | S.U_Not -> Ast.U_Not

  (* ------------------------------------------------------------------ *)
  let binop_of_pbinop (o : S.pbinop) : Ast.binop =
    match o with
    | S.B_And -> Ast.B_And
    | S.B_Or  -> Ast.B_Or
    | S.B_Add -> Ast.B_Add
    | S.B_Sub -> Ast.B_Sub
    | S.B_Mul -> Ast.B_Mul
    | S.B_Div -> Ast.B_Div
    | S.B_Lt  -> Ast.B_Lt
    | S.B_Gt  -> Ast.B_Gt
    | S.B_Le  -> Ast.B_Le
    | S.B_Ge  -> Ast.B_Ge

  (* ------------------------------------------------------------------ *)
  let tt_uniop (o : S.puniop) =
    let o = uniop_of_puniop o in
    let r =
      match o with
      | Ast.U_Not -> (Ast.TBool, Ast.TBool)
      | Ast.U_Neg -> (Ast.TInt , Ast.TInt )
    in (o, r)

  (* ------------------------------------------------------------------ *)
  let tt_binop (o : S.pbinop) =
    let o = binop_of_pbinop o in
    let r =
      match o with
      | Ast.B_And | Ast.B_Or ->
          ((Ast.TBool, Ast.TBool), Ast.TBool)
      | Ast.B_Add | Ast.B_Sub | Ast.B_Mul | Ast.B_Div ->
          ((Ast.TInt, Ast.TInt), Ast.TInt)
      | Ast.B_Lt  | Ast.B_Gt  | Ast.B_Le  | Ast.B_Ge  ->
          ((Ast.TInt, Ast.TInt), Ast.TBool)
    in (o, r)

  (* ------------------------------------------------------------------ *)
  let rec tt_expr (env : Env.env) (pe : S.pexpr) =
    match L.unloc pe with
    | S.PEBool b -> (Ast.EBool b, Some Ast.TBool)
    | S.PEInt  i -> (Ast.EInt  i, Some Ast.TInt)
    | S.PEVar  x -> let x, ty = tt_var env x in (Ast.EVar (x, ty), Some ty)

    | S.PEUni (o, pe) ->
        let (o, (oty1, orty)) = tt_uniop o in
        let e, ety = tt_expr env pe in
        tt_ty_assign ~to_:oty1 ~from:(L.loc pe, ety);
        (Ast.EUni (o, e), Some orty)

    | S.PEBin (o, (pe1, pe2)) ->
        let (o, ((oty1, oty2), orty)) = tt_binop o in
        let e1, ety1 = tt_expr env pe1 in
        let e2, ety2 = tt_expr env pe2 in
        tt_ty_assign ~to_:oty1 ~from:(L.loc pe1, ety1);
        tt_ty_assign ~to_:oty2 ~from:(L.loc pe2, ety2);
        (Ast.EBin (o, (e1, e2)), Some orty)

    | S.PEEq (neg, (pe1, pe2)) ->
        let e1, ety1 = tt_expr env pe1 in
        let e2, ety2 = tt_expr env pe2 in

        tt_ty_assign ~to_:Ast.TInt ~from:(L.loc pe1, ety1);
        tt_ty_assign ~to_:Ast.TInt ~from:(L.loc pe2, ety2);

        let aout = Ast.ECmp (e1, e2) in
        let aout = if neg then Ast.EUni (Ast.U_Neg, aout) else aout in
        (aout, Some Ast.TBool)

    | S.PECall (f, args) ->
        let f, sig_, retty = tt_fun env f in
        let args, argsty = List.split (List.map (fun ae ->
          let a, aty = tt_expr env ae in (a, (L.loc ae, aty))) args) in
        tt_ty_assign_sig sig_ (L.loc pe, argsty);
        (Ast.ECall (f, args), retty)

  (* ------------------------------------------------------------------ *)
  let rec tt_instr (env : Env.env) (rty : Ast.type_ option) (pi : S.pinstr) =
    match L.unloc pi with
    | S.PIEmpty ->
        None

    | S.PIAssign (None, pe) ->
        let e, _ = tt_expr env pe in
        Some (Ast.IAssign (None, e))

    | S.PIAssign (Some plv, pe) ->
        let ((_, lvty) as lv) = tt_var env plv in
        let e, ety = tt_expr env pe in
        tt_ty_assign ~to_:lvty ~from:(L.loc pi, ety);
        Some (Ast.IAssign (Some lv, e))

    | S.PIIf (pc, (pst, psf)) ->
        let c, cty = tt_expr env pc in
        let st = tt_stmt env rty pst in
        let sf =
          match psf with
          | Some psf -> tt_stmt env rty psf
          | None     -> []
        in
        tt_ty_assign ~to_:Ast.TBool ~from:(L.loc pc, cty);
        Some (Ast.IIf (c, (st, sf)))

    | S.PIWhile (pc, ps) ->
        let c, cty = tt_expr env pc in
        let s = tt_stmt env rty ps in
        tt_ty_assign ~to_:Ast.TBool ~from:(L.loc pc, cty);
        Some (Ast.IWhile (c, s))

    | S.PIReturn pe ->
        let re = omap (tt_expr env) pe in
        let ety = obind snd re in

        begin match rty, re with
        | None, None -> ()
        | None, Some _ -> raise (tyerror ~loc:(L.loc pi) RetInVoid)
        | Some rty, _ -> tt_ty_assign ~to_:rty ~from:(L.loc pi, ety) end;

        Some (Ast.IReturn re)

    | S.PIPrint pe ->
        let e, ety = tt_expr env pe in
        match ety with
        | None -> raise (tyerror ~loc:(L.loc pe) PrintInVoid)
        | Some ety -> Some (Ast.IPrint (e, ety))

  (* ------------------------------------------------------------------ *)
  and tt_stmt (env : Env.env) (rty : Ast.type_ option) (ps : S.pstmt) =
    let do1 = fun acc i ->
      match tt_instr env rty i with
      | Some i -> i :: acc
      | None   -> acc
    in List.rev (List.fold_left do1 [] (L.unloc ps))

  (* ------------------------------------------------------------------ *)
  let tt_procdef (env as env0 : Env.env) (ppd : S.pproc) =
    let { L.plloc = _lc; L.pldesc = ppd; } = ppd in
    let { L.plloc = lnc; L.pldesc = lnx; } = ppd.S.pr_name in

    if Env.Funs.exists env lnx then
      raise (tyerror ~loc:lnc (DuplicatedProcName lnx));

    let () =
      let allvars = 
        let lcls = fun (_, x) -> List.map fst x in
        let lcls = List.map lcls ppd.S.pr_locals in
        List.map snd ppd.S.pr_args @ List.flatten lcls in

      let check1 acc { L.pldesc = x; L.plloc = xlc; } =
        if SSet.mem x acc then
          raise (tyerror ~loc:xlc (DuplicatedVarName x));
        SSet.add x acc in

      ignore (List.fold_left check1 SSet.empty allvars : SSet.t)
    in        

    let tt_arg env (pty, pa) =
      let ax = L.unloc pa, tt_type env pty in
      (Env.Vars.push env ax, ax) in

    let tt_lcl env (pty, nms) =
      let ty = tt_type env pty in
      let for1 env (px, pe) =
        let lx = L.unloc px, ty in
        let tt_init pe =
          let e, ety = tt_expr env pe in
          tt_ty_assign ~to_:ty ~from:(L.loc pe, ety); (lx, e) in
        (Env.Vars.push env lx, (lx, omap tt_init pe)) in
      List.map_fold for1 env nms
    in

    let rty = omap (tt_type env) ppd.S.pr_retty in

    let env , args = List.map_fold tt_arg env ppd.S.pr_args in
    let env , lcls = List.map_fold tt_lcl env ppd.S.pr_locals in
    let lcls, init = List.split (List.flatten lcls) in

    let fdecl = (lnx, List.map snd args, rty) in

    let body =
      let do1 acc = function
        | None         -> acc
        | Some (lx, e) -> Ast.IAssign (Some lx, e) :: acc in

        List.rev (List.fold_left do1 [] init)
      @ tt_stmt (Env.Funs.push env fdecl) rty ppd.S.pr_body
    in

    let fdef  = {
      Ast.p_name  = lnx;
      Ast.p_args  = args;
      Ast.p_lcls  = lcls;
      Ast.p_retty = rty;
      Ast.p_body  = body;
    }

    in (Env.Funs.push env0 fdecl, fdef)

  (* ------------------------------------------------------------------ *)
  let tt_program (env : Env.env) (pprgm : S.pprogram) =
    List.map_fold
      (fun env (S.TProc ppd) -> tt_procdef env ppd)
      env pprgm
end

(* -------------------------------------------------------------------- *)
module HiAPI = struct
  (* ------------------------------------------------------------------ *)
  let pasm_of_string (s : string) =
    Io.parse_asm_of_string ~name:"" s

  (* ------------------------------------------------------------------ *)
  let pasm_of_file (s : string) =
    Io.parse_asm_of_file s

  (* ------------------------------------------------------------------ *)
  let pexpr_of_string (s : string) =
    Io.parse_expr (Io.from_string ~name:"" s)

  (* ------------------------------------------------------------------ *)
  let pstmt_of_string (s : string) =
    Io.parse_stmt (Io.from_string ~name:"" s)

  (* ------------------------------------------------------------------ *)
  let pprogram_of_string (s : string) =
    Io.parse_program (Io.from_string ~name:"" s)

  (* ------------------------------------------------------------------ *)
  let pprogram_of_file (input : string) =
    Io.parse_program (Io.from_file input)

  (* ------------------------------------------------------------------ *)
  let tt_expr map (ast : S.pexpr) =
    Typing.tt_expr (Typing.Env.of_bindings map) ast

  (* ------------------------------------------------------------------ *)
  let tt_stmt map (ast : S.pstmt) =
    Typing.tt_stmt (Typing.Env.of_bindings map) None ast

  (* ------------------------------------------------------------------ *)
  let tt_program (ast : S.pprogram) =
    snd (Typing.tt_program Typing.Env.empty ast)
end

(* -------------------------------------------------------------------- *)
type hierror =
  | HIE_ParseError of (L.t * string option)
  | HIE_TyError    of (L.t * Typing.tyerror)

exception HiError of hierror

(* -------------------------------------------------------------------- *)
let string_of_hierror hie =
  let buffer = Buffer.create 0 in
  let fmt    = Format.formatter_of_buffer buffer in

  let () =
    match hie with
    | HIE_ParseError (loc, None) ->
       Format.fprintf fmt "%s: parse error"
         (L.tostring loc)
  
    | HIE_ParseError (loc, Some msg) ->
       Format.fprintf fmt "%s: parse error: %s"
         (L.tostring loc) msg
  
    | HIE_TyError (loc, code) ->
       Format.fprintf fmt "%s: typing error: %a"
         (L.tostring loc) Typing.pp_tyerror code      

  in Format.pp_print_flush fmt (); Buffer.contents buffer

(* -------------------------------------------------------------------- *)
let wrap_hi (f : 'a -> 'b) (x : 'a) : 'b =
  try f x
  with
  | S.ParseError   args -> raise (HiError (HIE_ParseError args))
  | Typing.TyError args -> raise (HiError (HIE_TyError    args))

(* -------------------------------------------------------------------- *)
let pasm_of_string = wrap_hi HiAPI.pasm_of_string

(* -------------------------------------------------------------------- *)
let pasm_of_file = wrap_hi HiAPI.pasm_of_file

(* -------------------------------------------------------------------- *)
let pexpr_of_string = wrap_hi HiAPI.pexpr_of_string

(* -------------------------------------------------------------------- *)
let pstmt_of_string = wrap_hi HiAPI.pstmt_of_string

(* -------------------------------------------------------------------- *)
let pprogram_of_string = wrap_hi HiAPI.pprogram_of_string

(* -------------------------------------------------------------------- *)
let pprogram_of_file = wrap_hi HiAPI.pprogram_of_file

(* -------------------------------------------------------------------- *)
let tt_expr map = wrap_hi (HiAPI.tt_expr map)

(* -------------------------------------------------------------------- *)
let tt_stmt map = wrap_hi (HiAPI.tt_stmt map)

(* -------------------------------------------------------------------- *)
let tt_program = wrap_hi HiAPI.tt_program

(* -------------------------------------------------------------------- *)
let parse_and_tt_file_or_die (input : string) =
  try
    tt_program (pprogram_of_file input)
  with
  | HiError exn ->
      Format.eprintf "%s\n%!" (string_of_hierror exn);
      exit 1
