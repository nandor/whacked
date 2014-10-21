.section .data
.section .text
.global main
main:
	PUSH {LR}
	LDR R0, =0
	BL __println_bool
	LDR R0, =1
	BL __println_bool
	LDR R0, =1
	BL __println_bool
	LDR R0, =1
	BL __println_bool
	LDR R0, =0
	BL __println_bool
	LDR R0, =0
	BL __println_bool
	LDR R0, =0
	POP {PC}