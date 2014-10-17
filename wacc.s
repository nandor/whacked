main:
	PUSH {R4, R5, R6, LR}
L1:
	BL __read_int
	MOV R6, R0
	LDR R4, =5
	ADD R4, R6, R4
	LDR R5, =4
	CMP R4, R5
	BGT L8
	LDR R4, =12345678
	B L9
L8:
	LDR R4, =3
L9:
	LDR R5, =10
	CMP R4, R5
	BLT L1
	MOV R0, R4
	MOV R1, R6
	BL __aeabi_idiv
	MOV R4, R0
	MOV R0, R4
	BL __print_int
	LDR R4, =0
	MOV R0, R4
	POP {R4, R5, R6, PC}
test:
	PUSH {R4, LR}
	LDR R4, =2
	ADD R0, R0, R4
	ADD R0, R0, R1
	POP {R4, PC}