.section .data
.section .text
.global main
main:
	PUSH {LR}
	LDR R0, =1
	BL __print_bool
	BL __println
	LDR R0, =1
	BL __print_bool
	BL __println
	LDR R0, =0
	BL __print_bool
	BL __println