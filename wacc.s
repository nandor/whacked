.section .data
__msg0:
	.word 7
	.ascii "Correct"
.section .text
.global main
main:
	PUSH {LR}
	LDR R0, =__msg0
	BL __println_string
	LDR R0, =0
	POP {PC}