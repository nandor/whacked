main:
	PUSH {R4, LR}
	LDR R4, =-42
	MOV R0, R4
	BL __println_int
	POP {R4, PC}