type token =
  | UNKNOWN of (string)
  | INT of (int64 )
  | IDENT of (string)
  | ALU_ADD
  | ALU_SUB
  | ALU_MUL
  | ALU_DIV
  | ALU_NEG
  | ALU_LE
  | ALU_LT
  | ALU_LAND
  | ALU_LOR
  | ALU_LNOT
  | OC_ALU
  | OC_CALL
  | OC_DCD
  | OC_JMP
  | OC_JMPZ
  | OC_LOAD
  | OC_POP
  | OC_PRT
  | OC_PUSH
  | OC_RET
  | OC_STOP
  | OC_STORE
  | REG_FP
  | REG_SP
  | REG_IP
  | REG_R0
  | DOT
  | PERCENT
  | PLUS
  | MINUS
  | COLON
  | NL
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "asmParser.mly"
  open Syntax
  open Syntax.Location
  open Syntax.Asm

  let get_pos pos =
    match pos with
    | None ->
        (Parsing.symbol_start_pos (),
         Parsing.symbol_end_pos   ())
    | Some i ->
        (Parsing.rhs_start_pos i,
         Parsing.rhs_end_pos   i)

  let ploced ?pos (x : 'a) : 'a loced =
    let p1, p2 = get_pos pos in
    { pldesc = x; plloc = Location.make p1 p2; }

  let g_parse_error ?pos ?msg () =
    let p1, p2 = get_pos pos in
    Syntax.sparse_error ?msg (Location.make p1 p2)
# 63 "asmParser.ml"
let yytransl_const = [|
  260 (* ALU_ADD *);
  261 (* ALU_SUB *);
  262 (* ALU_MUL *);
  263 (* ALU_DIV *);
  264 (* ALU_NEG *);
  265 (* ALU_LE *);
  266 (* ALU_LT *);
  267 (* ALU_LAND *);
  268 (* ALU_LOR *);
  269 (* ALU_LNOT *);
  270 (* OC_ALU *);
  271 (* OC_CALL *);
  272 (* OC_DCD *);
  273 (* OC_JMP *);
  274 (* OC_JMPZ *);
  275 (* OC_LOAD *);
  276 (* OC_POP *);
  277 (* OC_PRT *);
  278 (* OC_PUSH *);
  279 (* OC_RET *);
  280 (* OC_STOP *);
  281 (* OC_STORE *);
  282 (* REG_FP *);
  283 (* REG_SP *);
  284 (* REG_IP *);
  285 (* REG_R0 *);
  286 (* DOT *);
  287 (* PERCENT *);
  288 (* PLUS *);
  289 (* MINUS *);
  290 (* COLON *);
  291 (* NL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* UNKNOWN *);
  258 (* INT *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\005\000\005\000\006\000\
\006\000\007\000\008\000\008\000\009\000\002\000\002\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\003\000\001\000\000\000\001\000\001\000\001\000\001\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\002\000\001\000\002\000\002\000\001\000\
\002\000\002\000\002\000\002\000\001\000\001\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\000\000\000\014\000\000\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\013\000\
\031\000\026\000\035\000\036\000\012\000\000\000\011\000\033\000\
\028\000\030\000\034\000\005\000\000\000\004\000\000\000\039\000\
\006\000\007\000\000\000\015\000\001\000\000\000\000\000\010\000\
\008\000\009\000"

let yydgoto = "\002\000\
\018\000\019\000\047\000\045\000\051\000\056\000\039\000\040\000\
\033\000\020\000\021\000"

let yysindex = "\255\255\
\006\255\000\000\000\000\000\000\035\255\009\255\016\255\009\255\
\009\255\007\255\244\254\000\000\007\255\000\000\000\000\244\254\
\029\255\000\000\254\254\000\000\034\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\240\254\000\000\000\000\
\000\000\000\000\000\000\000\000\001\255\000\000\037\000\000\000\
\000\000\000\000\238\254\000\000\000\000\047\255\048\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\051\255\052\255\053\255\054\255\
\055\255\056\255\001\000\002\000\003\000\004\000\005\000\057\255\
\000\000\000\000\058\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\253\255\046\000\
\008\000\000\000\000\000"

let yytablesize = 296
let yytable = "\001\000\
\027\000\037\000\029\000\032\000\038\000\003\000\004\000\041\000\
\037\000\049\000\050\000\032\000\043\000\054\000\055\000\035\000\
\036\000\034\000\038\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\044\000\
\046\000\048\000\052\000\017\000\053\000\038\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\057\000\058\000\041\000\042\000\043\000\044\000\045\000\046\000\
\052\000\003\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\048\000\049\000\050\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\037\000\029\000\032\000\038\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\000\000\000\001\001\001\011\000\
\002\001\026\001\027\001\003\001\016\000\032\001\033\001\008\000\
\009\000\002\001\031\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\003\001\
\035\001\000\001\034\001\030\001\000\000\031\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\002\001\002\001\000\001\000\001\000\001\000\001\000\001\000\001\
\000\001\000\000\013\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\000\001\000\001\000\001\000\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\035\001\035\001\035\001\035\001\035\001"

let yynames_const = "\
  ALU_ADD\000\
  ALU_SUB\000\
  ALU_MUL\000\
  ALU_DIV\000\
  ALU_NEG\000\
  ALU_LE\000\
  ALU_LT\000\
  ALU_LAND\000\
  ALU_LOR\000\
  ALU_LNOT\000\
  OC_ALU\000\
  OC_CALL\000\
  OC_DCD\000\
  OC_JMP\000\
  OC_JMPZ\000\
  OC_LOAD\000\
  OC_POP\000\
  OC_PRT\000\
  OC_PUSH\000\
  OC_RET\000\
  OC_STOP\000\
  OC_STORE\000\
  REG_FP\000\
  REG_SP\000\
  REG_IP\000\
  REG_R0\000\
  DOT\000\
  PERCENT\000\
  PLUS\000\
  MINUS\000\
  COLON\000\
  NL\000\
  EOF\000\
  "

let yynames_block = "\
  UNKNOWN\000\
  INT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'entry_r) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'maybe_nl) in
    Obj.repr(
# 67 "asmParser.mly"
    ( _1 )
# 291 "asmParser.ml"
               : Syntax.Asm.pentry))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "asmParser.mly"
    ( g_parse_error () )
# 297 "asmParser.ml"
               : Syntax.Asm.pentry))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "asmParser.mly"
              ()
# 303 "asmParser.ml"
               : 'maybe_nl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "asmParser.mly"
     ()
# 309 "asmParser.ml"
               : 'maybe_nl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "asmParser.mly"
        ( ploced _1 )
# 316 "asmParser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "asmParser.mly"
         ( `FP )
# 322 "asmParser.ml"
               : 'saddr_reg))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "asmParser.mly"
         ( `SP )
# 328 "asmParser.ml"
               : 'saddr_reg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64 ) in
    Obj.repr(
# 88 "asmParser.mly"
            (  _2 )
# 335 "asmParser.ml"
               : 'saddr_off))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64 ) in
    Obj.repr(
# 89 "asmParser.mly"
            ( Int64.neg _2 )
# 342 "asmParser.ml"
               : 'saddr_off))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'saddr_reg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'saddr_off) in
    Obj.repr(
# 93 "asmParser.mly"
                              ( (_2, _3) )
# 350 "asmParser.ml"
               : 'staddr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'staddr) in
    Obj.repr(
# 98 "asmParser.mly"
    ( `Stack _1 )
# 357 "asmParser.ml"
               : 'saddr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int64 ) in
    Obj.repr(
# 101 "asmParser.mly"
    ( `Imm _1 )
# 364 "asmParser.ml"
               : 'saddr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "asmParser.mly"
        ( P_ALbl _1 )
# 371 "asmParser.ml"
               : 'addr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 110 "asmParser.mly"
    ( P_Instr (ploced _1) )
# 378 "asmParser.ml"
               : 'entry_r))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    Obj.repr(
# 113 "asmParser.mly"
    ( P_Label _2 )
# 385 "asmParser.ml"
               : 'entry_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "asmParser.mly"
                   ( P_ALU `ADD  )
# 391 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "asmParser.mly"
                   ( P_ALU `SUB  )
# 397 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "asmParser.mly"
                   ( P_ALU `MUL  )
# 403 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "asmParser.mly"
                   ( P_ALU `DIV  )
# 409 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "asmParser.mly"
                   ( P_ALU `NEG  )
# 415 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "asmParser.mly"
                   ( P_ALU `LE   )
# 421 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "asmParser.mly"
                   ( P_ALU `LT   )
# 427 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "asmParser.mly"
                   ( P_ALU `LAND )
# 433 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "asmParser.mly"
                   ( P_ALU `LOR  )
# 439 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "asmParser.mly"
                   ( P_ALU `LNOT )
# 445 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int64 ) in
    Obj.repr(
# 129 "asmParser.mly"
    ( P_DCD _2 )
# 452 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "asmParser.mly"
    ( P_POP None )
# 458 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'staddr) in
    Obj.repr(
# 135 "asmParser.mly"
    ( P_POP (Some _2) )
# 465 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "asmParser.mly"
    ( P_PUSH None )
# 471 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'saddr) in
    Obj.repr(
# 141 "asmParser.mly"
    ( P_PUSH (Some _2) )
# 478 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'addr) in
    Obj.repr(
# 144 "asmParser.mly"
    ( P_CALL _2 )
# 485 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "asmParser.mly"
    ( P_RET )
# 491 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'saddr) in
    Obj.repr(
# 150 "asmParser.mly"
    ( P_LOAD _2 )
# 498 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'staddr) in
    Obj.repr(
# 153 "asmParser.mly"
    ( P_STORE _2 )
# 505 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'addr) in
    Obj.repr(
# 156 "asmParser.mly"
    ( P_JMP _2 )
# 512 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'addr) in
    Obj.repr(
# 159 "asmParser.mly"
    ( P_JMPZ _2 )
# 519 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "asmParser.mly"
    ( P_PRT )
# 525 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "asmParser.mly"
    ( P_STOP )
# 531 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'popcode) in
    Obj.repr(
# 168 "asmParser.mly"
    ( g_parse_error ~pos:1 ~msg:"invalid argument for opcode" () )
# 538 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 171 "asmParser.mly"
    ( g_parse_error ~pos:1 ~msg:("unknown opcode: " ^ _1) () )
# 545 "asmParser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "asmParser.mly"
           ( `ALU   )
# 551 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 176 "asmParser.mly"
           ( `CALL  )
# 557 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "asmParser.mly"
           ( `DCD   )
# 563 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "asmParser.mly"
           ( `JMP   )
# 569 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "asmParser.mly"
           ( `JMPZ  )
# 575 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "asmParser.mly"
           ( `LOAD  )
# 581 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 181 "asmParser.mly"
           ( `POP   )
# 587 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "asmParser.mly"
           ( `PRT   )
# 593 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "asmParser.mly"
           ( `PUSH  )
# 599 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "asmParser.mly"
           ( `RET   )
# 605 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "asmParser.mly"
           ( `STOP  )
# 611 "asmParser.ml"
               : 'popcode))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "asmParser.mly"
           ( `STORE )
# 617 "asmParser.ml"
               : 'popcode))
(* Entry entry *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let entry (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.Asm.pentry)
