.section .data
__msg5:
	.word 18
	.ascii "final value of x: "
__msg4:
	.word 0
	.ascii ""
__msg3:
	.word 3
	.ascii "(+)"
__msg2:
	.word 20
	.ascii "Initial value of x: "
__msg1:
	.word 25
	.ascii "Enter the second number: "
__msg0:
	.word 24
	.ascii "Enter the first number: "
.section .text
.global main
main:
	PUSH {R4, R5, R6, LR}
	LDR R4, =__msg0
	MOV R0, R4
	BL __print_string
	BL __read_int
	MOV R4, R0
	LDR R0, =__msg1
	BL __print_string
	BL __read_int
	MOV R5, R0
	LDR R0, =__msg2
	BL __print_string
	MOV R0, R4
	BL __print_int
	BL __println
	LDR R0, =0
	CMP R5, R0
	BGT L20
L12:
	LDR R0, =__msg3
	BL __print_string
	LDR R0, =1
	ADD R4, R4, R0
	LDR R0, =1
	SUB R6, R5, R0
	LDR R0, =0
	CMP R6, R0
	BGT L12
L20:
	LDR R0, =__msg4
	BL __print_string
	BL __println
	LDR R0, =__msg5
	BL __print_string
	MOV R0, R4
	BL __print_int
	BL __println
	LDR R4, =0
	MOV R0, R4
	POP {R4, R5, R6, PC}