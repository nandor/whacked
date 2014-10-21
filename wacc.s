main:
	PUSH {R4, R5, R6, LR}
	LDR R5, =1
	MOV R0, R5
	BL __println_bool
	LDR R6, =1
	MOV R0, R6
	BL __println_bool
	LDR R4, =0
	MOV R0, R4
	BL __println_bool
	POP {R4, R5, R6, PC}