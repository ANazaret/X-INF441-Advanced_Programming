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

val entry :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.Asm.pentry
