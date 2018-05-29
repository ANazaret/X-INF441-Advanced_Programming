.entry:
	CALL 	main
	STOP
.fact:
	PUSH 	0x01
.fact_loop:
	PUSH 	%FP-3
	PUSH 	0x00
	ALU  	LT
	POP
	JMPZ 	fact_end
	PUSH 	%FP-3
	PUSH 	%FP+0
	ALU  	MUL
	POP  	%FP+0
	PUSH 	0x01
	PUSH 	%FP-3
	ALU  	SUB
	POP  	%FP-3
	JMP  	fact_loop
.fact_end:
	LOAD 	%FP+0
	RET
.main:
	PUSH 	0x04
	CALL 	fact
	DCD  	0x01
	PRT
.main_end:
	RET
