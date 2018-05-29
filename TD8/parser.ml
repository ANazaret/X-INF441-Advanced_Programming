type token =
  | IDENT of (Syntax.symbol)
  | INT64 of (Int64.t)
  | BOOL
  | ELSE
  | FALSE
  | IF
  | INT
  | PRINT
  | RETURN
  | TRUE
  | VOID
  | WHILE
  | EQ
  | LT
  | GT
  | LE
  | GE
  | EQEQ
  | BANGEQ
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PIPEPIPE
  | AMPAMP
  | EOF
  | BANG
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  open Syntax.Location

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
# 59 "parser.ml"
let yytransl_const = [|
  259 (* BOOL *);
  260 (* ELSE *);
  261 (* FALSE *);
  262 (* IF *);
  263 (* INT *);
  264 (* PRINT *);
  265 (* RETURN *);
  266 (* TRUE *);
  267 (* VOID *);
  268 (* WHILE *);
  269 (* EQ *);
  270 (* LT *);
  271 (* GT *);
  272 (* LE *);
  273 (* GE *);
  274 (* EQEQ *);
  275 (* BANGEQ *);
  276 (* PLUS *);
  277 (* MINUS *);
  278 (* STAR *);
  279 (* SLASH *);
  280 (* PIPEPIPE *);
  281 (* AMPAMP *);
    0 (* EOF *);
  282 (* BANG *);
  283 (* COMMA *);
  284 (* SEMICOLON *);
  285 (* LPAREN *);
  286 (* RPAREN *);
  287 (* LBRACE *);
  288 (* RBRACE *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* INT64 *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\008\000\009\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\012\000\012\000\013\000\013\000\014\000\014\000\016\000\016\000\
\016\000\016\000\016\000\016\000\017\000\017\000\017\000\017\000\
\006\000\006\000\020\000\020\000\021\000\022\000\022\000\023\000\
\023\000\024\000\025\000\025\000\026\000\026\000\027\000\028\000\
\028\000\029\000\029\000\030\000\031\000\007\000\015\000\019\000\
\005\000\018\000\032\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\002\000\001\000\000\000\002\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\000\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\002\000\
\004\000\003\000\002\000\003\000\001\000\005\000\007\000\005\000\
\000\000\002\000\003\000\001\000\002\000\000\000\001\000\001\000\
\003\000\003\000\001\000\003\000\001\000\003\000\003\000\000\000\
\001\000\001\000\002\000\002\000\006\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\002\000\036\000\035\000\038\000\
\076\000\000\000\000\000\071\000\000\000\037\000\075\000\070\000\
\004\000\009\000\014\000\013\000\012\000\000\000\000\000\000\000\
\077\000\000\000\000\000\073\000\006\000\000\000\000\000\000\000\
\000\000\039\000\078\000\000\000\000\000\000\000\000\000\045\000\
\072\000\000\000\001\000\008\000\000\000\017\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\043\000\000\000\000\000\040\000\005\000\000\000\050\000\
\000\000\000\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\023\000\000\000\000\000\000\000\
\000\000\032\000\000\000\042\000\044\000\000\000\000\000\000\000\
\000\000\000\000\055\000\000\000\000\000\030\000\000\000\000\000\
\041\000\053\000\000\000\058\000\000\000\000\000\000\000\065\000\
\000\000\034\000\000\000\000\000\052\000\074\000\048\000\057\000\
\000\000\000\000\000\000\067\000\068\000\069\000\000\000\000\000\
\000\000\000\000\063\000\051\000\047\000\000\000\062\000"

let yydgoto = "\004\000\
\009\000\025\000\035\000\010\000\036\000\037\000\011\000\027\000\
\039\000\028\000\089\000\090\000\012\000\013\000\014\000\040\000\
\041\000\116\000\042\000\118\000\097\000\098\000\099\000\074\000\
\122\000\123\000\110\000\111\000\112\000\113\000\015\000\016\000"

let yysindex = "\064\000\
\042\255\018\255\201\255\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\045\255\000\000\021\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\213\255\213\255\213\255\
\000\000\150\000\254\254\000\000\000\000\004\255\213\255\087\255\
\007\255\000\000\000\000\224\000\041\000\254\254\030\255\000\000\
\000\000\175\000\000\000\000\000\017\255\000\000\000\000\042\001\
\027\255\213\255\213\255\213\255\213\255\213\255\213\255\213\255\
\213\255\213\255\213\255\213\255\213\255\000\000\213\255\213\255\
\239\000\000\000\254\000\213\255\000\000\000\000\213\255\000\000\
\051\255\020\255\000\000\041\255\041\255\041\255\041\255\109\255\
\109\255\240\254\240\254\000\000\000\000\066\001\078\001\028\001\
\029\255\000\000\230\255\000\000\000\000\191\000\013\001\021\255\
\047\255\048\255\000\000\051\255\213\255\000\000\169\255\169\255\
\000\000\000\000\051\255\000\000\021\255\051\255\175\000\000\000\
\053\255\000\000\175\000\073\255\000\000\000\000\000\000\000\000\
\078\255\060\255\065\255\000\000\000\000\000\000\064\255\169\255\
\213\255\021\255\000\000\000\000\000\000\042\001\000\000"

let yyrindex = "\000\000\
\098\000\000\000\101\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\098\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\209\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\054\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\079\255\000\000\000\000\068\000\081\000\094\000\107\000\114\000\
\121\000\038\000\055\000\000\000\000\000\133\000\128\000\088\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\092\255\000\000\000\000\003\255\075\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\255\072\255\000\000\
\000\000\000\000\072\255\001\000\000\000\000\000\000\000\000\000\
\245\254\089\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\116\000\088\000\214\255\000\000\057\000\
\000\000\110\000\034\000\000\000\000\000\000\000\186\255\000\000\
\000\000\162\255\178\255\000\000\000\000\000\000\029\000\000\000\
\000\000\007\000\000\000\000\000\044\000\000\000\000\000\000\000"

let yytablesize = 613
let yytable = "\072\000\
\046\000\049\000\096\000\064\000\064\000\058\000\059\000\064\000\
\064\000\119\000\064\000\064\000\064\000\043\000\064\000\059\000\
\059\000\017\000\018\000\019\000\015\000\018\000\020\000\064\000\
\117\000\117\000\063\000\021\000\064\000\109\000\064\000\064\000\
\064\000\133\000\064\000\068\000\096\000\020\000\022\000\109\000\
\070\000\005\000\071\000\023\000\006\000\073\000\024\000\006\000\
\007\000\117\000\100\000\007\000\008\000\006\000\021\000\008\000\
\075\000\007\000\102\000\038\000\056\000\057\000\058\000\059\000\
\001\000\002\000\003\000\024\000\125\000\045\000\060\000\060\000\
\127\000\107\000\066\000\066\000\128\000\108\000\066\000\066\000\
\025\000\066\000\066\000\066\000\126\000\066\000\130\000\018\000\
\019\000\026\000\129\000\020\000\131\000\026\000\066\000\132\000\
\021\000\007\000\038\000\066\000\049\000\066\000\066\000\049\000\
\031\000\066\000\027\000\022\000\054\000\046\000\047\000\048\000\
\023\000\028\000\066\000\024\000\061\000\033\000\065\000\067\000\
\029\000\056\000\050\000\051\000\052\000\053\000\044\000\018\000\
\056\000\057\000\058\000\059\000\019\000\049\000\114\000\120\000\
\135\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\062\000\088\000\091\000\
\106\000\124\000\000\000\094\000\000\000\000\000\095\000\038\000\
\038\000\000\000\000\000\000\000\000\000\121\000\000\000\038\000\
\000\000\018\000\019\000\038\000\000\000\020\000\030\000\000\000\
\031\000\032\000\021\000\000\000\033\000\000\000\000\000\000\000\
\038\000\000\000\121\000\000\000\088\000\022\000\000\000\000\000\
\000\000\000\000\023\000\000\000\034\000\024\000\000\000\115\000\
\029\000\018\000\019\000\000\000\000\000\020\000\030\000\000\000\
\031\000\032\000\021\000\000\000\033\000\018\000\019\000\000\000\
\134\000\020\000\000\000\000\000\000\000\022\000\021\000\000\000\
\000\000\000\000\023\000\000\000\034\000\024\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\024\000\000\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\046\000\046\000\103\000\000\000\046\000\046\000\000\000\
\046\000\046\000\046\000\000\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\046\000\000\000\000\000\
\000\000\000\000\046\000\000\000\046\000\046\000\000\000\000\000\
\046\000\049\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\000\000\015\000\
\015\000\000\000\015\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\000\000\000\000\020\000\020\000\000\000\
\020\000\020\000\000\000\020\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\000\000\000\000\021\000\021\000\
\000\000\021\000\021\000\000\000\021\000\024\000\024\000\000\000\
\000\000\000\000\000\000\024\000\024\000\000\000\024\000\024\000\
\000\000\024\000\025\000\025\000\000\000\000\000\000\000\000\000\
\025\000\025\000\000\000\025\000\025\000\000\000\025\000\026\000\
\026\000\000\000\000\000\000\000\000\000\026\000\026\000\000\000\
\026\000\026\000\000\000\026\000\027\000\027\000\000\000\000\000\
\000\000\000\000\027\000\027\000\000\000\027\000\027\000\000\000\
\027\000\028\000\028\000\000\000\028\000\028\000\000\000\028\000\
\029\000\029\000\000\000\029\000\029\000\000\000\029\000\018\000\
\018\000\000\000\018\000\018\000\019\000\018\000\000\000\019\000\
\019\000\000\000\019\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\018\000\
\019\000\000\000\000\000\020\000\030\000\000\000\031\000\032\000\
\021\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\023\000\000\000\034\000\024\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\000\000\000\000\000\000\000\000\104\000\010\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\000\000\000\000\015\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\000\000\000\000\069\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\000\000\000\000\092\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\093\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\060\000\061\000\000\000\000\000\
\105\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\000\000\101\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\061\000\073\000\073\000\073\000\073\000\073\000\
\073\000\073\000\073\000\073\000\073\000\073\000\073\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\000\000\061\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000"

let yycheck = "\042\000\
\000\000\000\000\073\000\001\001\002\001\022\001\023\001\005\001\
\006\001\104\000\008\001\009\001\010\001\000\000\012\001\027\001\
\028\001\000\001\001\001\002\001\000\000\001\001\005\001\021\001\
\103\000\104\000\029\001\010\001\026\001\100\000\028\001\029\001\
\029\001\128\000\032\001\029\001\107\000\000\000\021\001\110\000\
\000\000\000\001\013\001\026\001\003\001\029\001\029\001\003\001\
\007\001\128\000\031\001\007\001\011\001\003\001\000\000\011\001\
\030\001\007\001\030\001\003\000\020\001\021\001\022\001\023\001\
\001\000\002\000\003\000\000\000\111\000\013\000\027\001\028\001\
\115\000\027\001\001\001\002\001\004\001\030\001\005\001\006\001\
\000\000\008\001\009\001\010\001\032\001\012\001\027\001\001\001\
\002\001\002\000\013\001\005\001\028\001\000\000\021\001\032\001\
\010\001\000\000\042\000\026\001\000\000\028\001\029\001\032\001\
\030\001\032\001\000\000\021\001\030\001\022\000\023\000\024\000\
\026\001\000\000\028\001\029\001\028\001\030\001\031\000\032\000\
\000\000\030\001\014\001\015\001\016\001\017\001\011\000\000\000\
\020\001\021\001\022\001\023\001\000\000\024\000\101\000\107\000\
\130\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\000\000\063\000\064\000\
\096\000\110\000\255\255\068\000\255\255\255\255\071\000\103\000\
\104\000\255\255\255\255\255\255\255\255\109\000\255\255\111\000\
\255\255\001\001\002\001\115\000\255\255\005\001\006\001\255\255\
\008\001\009\001\010\001\255\255\012\001\255\255\255\255\255\255\
\128\000\255\255\130\000\255\255\101\000\021\001\255\255\255\255\
\255\255\255\255\026\001\255\255\028\001\029\001\255\255\031\001\
\000\001\001\001\002\001\255\255\255\255\005\001\006\001\255\255\
\008\001\009\001\010\001\255\255\012\001\001\001\002\001\255\255\
\129\000\005\001\255\255\255\255\255\255\021\001\010\001\255\255\
\255\255\255\255\026\001\255\255\028\001\029\001\255\255\255\255\
\255\255\021\001\255\255\255\255\255\255\255\255\026\001\255\255\
\255\255\029\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\255\255\
\255\255\001\001\002\001\030\001\255\255\005\001\006\001\255\255\
\008\001\009\001\010\001\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\255\255\
\255\255\255\255\026\001\255\255\028\001\029\001\255\255\255\255\
\032\001\032\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\255\255\027\001\
\028\001\255\255\030\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\024\001\025\001\255\255\
\027\001\028\001\255\255\030\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\255\255\255\255\024\001\025\001\
\255\255\027\001\028\001\255\255\030\001\018\001\019\001\255\255\
\255\255\255\255\255\255\024\001\025\001\255\255\027\001\028\001\
\255\255\030\001\018\001\019\001\255\255\255\255\255\255\255\255\
\024\001\025\001\255\255\027\001\028\001\255\255\030\001\018\001\
\019\001\255\255\255\255\255\255\255\255\024\001\025\001\255\255\
\027\001\028\001\255\255\030\001\018\001\019\001\255\255\255\255\
\255\255\255\255\024\001\025\001\255\255\027\001\028\001\255\255\
\030\001\024\001\025\001\255\255\027\001\028\001\255\255\030\001\
\024\001\025\001\255\255\027\001\028\001\255\255\030\001\024\001\
\025\001\255\255\027\001\028\001\024\001\030\001\255\255\027\001\
\028\001\255\255\030\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\001\001\
\002\001\255\255\255\255\005\001\006\001\255\255\008\001\009\001\
\010\001\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\021\001\255\255\255\255\255\255\255\255\
\026\001\255\255\028\001\029\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\255\255\255\255\255\255\255\255\030\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\255\255\255\255\028\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\255\255\255\255\028\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\255\255\255\255\028\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\255\255\
\255\255\028\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\255\255\255\255\
\028\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\255\255\027\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\025\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001"

let yynames_const = "\
  BOOL\000\
  ELSE\000\
  FALSE\000\
  IF\000\
  INT\000\
  PRINT\000\
  RETURN\000\
  TRUE\000\
  VOID\000\
  WHILE\000\
  EQ\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  EQEQ\000\
  BANGEQ\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  PIPEPIPE\000\
  AMPAMP\000\
  EOF\000\
  BANG\000\
  COMMA\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  "

let yynames_block = "\
  IDENT\000\
  INT64\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 67 "parser.mly"
    ( _1 )
# 397 "parser.ml"
               : Syntax.pprogram))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
   ( g_parse_error () )
# 403 "parser.ml"
               : Syntax.pprogram))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
    ( _1 )
# 410 "parser.ml"
               : Syntax.pexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
   ( g_parse_error () )
# 416 "parser.ml"
               : Syntax.pexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instrs_0) in
    Obj.repr(
# 83 "parser.mly"
    ( ploced _1 )
# 423 "parser.ml"
               : Syntax.pstmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
   ( g_parse_error () )
# 429 "parser.ml"
               : Syntax.pstmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
              ( [] )
# 435 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topprog) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 91 "parser.mly"
                  ( _1 :: _2 )
# 443 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.symbol) in
    Obj.repr(
# 95 "parser.mly"
        ( ploced _1 )
# 450 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 99 "parser.mly"
        ( _1 )
# 457 "parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_r) in
    Obj.repr(
# 104 "parser.mly"
    ( _2 )
# 464 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
    ( PEBool true  )
# 470 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
    ( PEBool false )
# 476 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Int64.t) in
    Obj.repr(
# 113 "parser.mly"
    ( PEInt _1 )
# 483 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 116 "parser.mly"
    ( PEVar _1 )
# 490 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
    ( PEUni (U_Not, _2) )
# 497 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
    ( PEUni (U_Neg, _2) )
# 504 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
    ( PEBin (B_And, (_1, _3)) )
# 512 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
    ( PEBin (B_Or, (_1, _3)) )
# 520 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
    ( PEBin (B_Add, (_1, _3)) )
# 528 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
    ( PEBin (B_Sub, (_1, _3)) )
# 536 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
    ( PEBin (B_Mul, (_1, _3)) )
# 544 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
    ( PEBin (B_Div, (_1, _3)) )
# 552 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
    ( PEBin (B_Lt, (_1, _3)) )
# 560 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
    ( PEBin (B_Gt, (_1, _3)) )
# 568 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
    ( PEBin (B_Le, (_1, _3)) )
# 576 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
    ( PEBin (B_Ge, (_1, _3)) )
# 584 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
    ( PEEq (false, (_1, _3)) )
# 592 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
    ( PEEq (true, (_1, _3)) )
# 600 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs_0_comma) in
    Obj.repr(
# 161 "parser.mly"
    ( PECall (_1, _3) )
# 608 "parser.ml"
               : 'expr_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
                ( [] )
# 614 "parser.ml"
               : 'exprs_0_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprs_1_comma) in
    Obj.repr(
# 166 "parser.mly"
                ( _1 )
# 621 "parser.ml"
               : 'exprs_0_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
    ( [_1] )
# 628 "parser.ml"
               : 'exprs_1_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs_0_comma) in
    Obj.repr(
# 173 "parser.mly"
    ( _1 :: _3 )
# 636 "parser.ml"
               : 'exprs_1_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
       ( PTInt  )
# 642 "parser.ml"
               : 'type_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "parser.mly"
       ( PTBool )
# 648 "parser.ml"
               : 'type_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 181 "parser.mly"
        ( Some _1 )
# 655 "parser.ml"
               : 'rtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "parser.mly"
        ( None    )
# 661 "parser.ml"
               : 'rtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "parser.mly"
    ( PIEmpty )
# 667 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
    ( PIAssign (None, _1) )
# 674 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
    ( PIAssign (Some _1, _3) )
# 682 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
    ( PIPrint _2 )
# 689 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 199 "parser.mly"
    ( PIReturn None )
# 695 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 202 "parser.mly"
    ( PIReturn (Some _2) )
# 702 "parser.ml"
               : 'binstr_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binstr_r) in
    Obj.repr(
# 206 "parser.mly"
    ( _1 )
# 709 "parser.ml"
               : 'instr_r))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sblock) in
    Obj.repr(
# 209 "parser.mly"
    ( PIIf (_3, (_5, None)) )
# 717 "parser.ml"
               : 'instr_r))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'sblock) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'sblock) in
    Obj.repr(
# 212 "parser.mly"
    ( PIIf (_3, (_5, Some _7)) )
# 726 "parser.ml"
               : 'instr_r))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sblock) in
    Obj.repr(
# 215 "parser.mly"
    ( PIWhile (_3, _5) )
# 734 "parser.ml"
               : 'instr_r))
; (fun __caml_parser_env ->
    Obj.repr(
# 219 "parser.mly"
                 ( [] )
# 740 "parser.ml"
               : 'instrs_0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instrs_0) in
    Obj.repr(
# 220 "parser.mly"
                 ( _1 :: _2 )
# 748 "parser.ml"
               : 'instrs_0))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'instrs_0) in
    Obj.repr(
# 225 "parser.mly"
    ( _2 )
# 755 "parser.ml"
               : 'sblock_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr) in
    Obj.repr(
# 228 "parser.mly"
    ( [_1] )
# 762 "parser.ml"
               : 'sblock_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 232 "parser.mly"
              ( (_1, _2) )
# 770 "parser.ml"
               : 'parg))
; (fun __caml_parser_env ->
    Obj.repr(
# 235 "parser.mly"
                ( [] )
# 776 "parser.ml"
               : 'pargs_0_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pargs_1_comma) in
    Obj.repr(
# 236 "parser.mly"
                ( _1 )
# 783 "parser.ml"
               : 'pargs_0_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parg) in
    Obj.repr(
# 239 "parser.mly"
       ( [_1] )
# 790 "parser.ml"
               : 'pargs_1_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pargs_1_comma) in
    Obj.repr(
# 240 "parser.mly"
                           ( _1 :: _3 )
# 798 "parser.ml"
               : 'pargs_1_comma))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pargs_0_comma) in
    Obj.repr(
# 243 "parser.mly"
                              ( _2 )
# 805 "parser.ml"
               : 'pargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 246 "parser.mly"
                ( (_1, None) )
# 812 "parser.ml"
               : 'plocal1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 247 "parser.mly"
                ( (_1, Some _3) )
# 820 "parser.ml"
               : 'plocal1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plocal1) in
    Obj.repr(
# 250 "parser.mly"
          ( [_1] )
# 827 "parser.ml"
               : 'plocal1_1_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plocal1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'plocal1_1_comma) in
    Obj.repr(
# 251 "parser.mly"
                                ( _1 :: _3 )
# 835 "parser.ml"
               : 'plocal1_1_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'plocal1_1_comma) in
    Obj.repr(
# 254 "parser.mly"
                                  ( (_1, _2) )
# 843 "parser.ml"
               : 'plocal))
; (fun __caml_parser_env ->
    Obj.repr(
# 257 "parser.mly"
              ( [] )
# 849 "parser.ml"
               : 'plocals_0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plocals_1) in
    Obj.repr(
# 258 "parser.mly"
              ( _1 )
# 856 "parser.ml"
               : 'plocals_0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plocal) in
    Obj.repr(
# 261 "parser.mly"
         ( [_1] )
# 863 "parser.ml"
               : 'plocals_1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'plocal) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'plocals_1) in
    Obj.repr(
# 262 "parser.mly"
                   ( _1 :: _2 )
# 871 "parser.ml"
               : 'plocals_1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'plocals_0) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'instrs_0) in
    Obj.repr(
# 265 "parser.mly"
                     ( (_1, ploced ~pos:2 _2) )
# 879 "parser.ml"
               : 'pbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'rtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'pargs) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'pbody) in
    Obj.repr(
# 268 "parser.mly"
                                        (
    { pr_name   = _2;
      pr_retty  = _1;
      pr_args   = _3;
      pr_locals = fst _5;
      pr_body   = snd _5; } )
# 894 "parser.ml"
               : 'proc_r))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 277 "parser.mly"
       ( TProc _1 )
# 901 "parser.ml"
               : 'topprog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_r) in
    Obj.repr(
# 280 "parser.mly"
                  ( ploced _1 )
# 908 "parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'instr_r) in
    Obj.repr(
# 281 "parser.mly"
                  ( ploced _1 )
# 915 "parser.ml"
               : 'instr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_r) in
    Obj.repr(
# 282 "parser.mly"
                  ( ploced _1 )
# 922 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sblock_r) in
    Obj.repr(
# 283 "parser.mly"
                  ( ploced _1 )
# 929 "parser.ml"
               : 'sblock))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_r) in
    Obj.repr(
# 284 "parser.mly"
                  ( ploced _1 )
# 936 "parser.ml"
               : 'proc))
(* Entry xprogram *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry xexpr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry xstmt *)
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
let xprogram (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.pprogram)
let xexpr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Syntax.pexpr)
let xstmt (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Syntax.pstmt)
