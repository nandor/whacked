.section .data
__msg1:
	.word 4
	.ascii " is "
__msg0:
	.word 4
	.ascii " is "
.section .text
.global main
main:
	PUSH {R4, LR}
	LDR R0, =97
	LDR R4, =99
	BL __print_char
	LDR R0, =__msg0
	BL __print_string
	LDR R0, =97
	BL __print_int
	BL __println
	MOV R0, R4
	BL __print_int
	LDR R0, =__msg1
	BL __print_string
	MOV R0, R4
	BL __print_char
	BL __println
	LDR R4, =0
	MOV R0, R4
	POP {R4, PC}