main:
	PUSH {R4, LR}
	LDR R4, =-5
	LDR R1, =-3
	MOV R0, R4
	BL __aeabi_idivmod
	MOV R4, R1
	MOV R0, R4
	BL __println_int
	POP {R4, PC}