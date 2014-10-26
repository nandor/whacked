.section .data
.long 18
msg_0:
.ascii "You have entered: "
.long 24
msg_1:
.ascii "There is only 1 integer."
.long 23
msg_10:
.ascii "Here are the integers: "
.long 35
msg_11:
.ascii "Please enter an integer to remove: "
.long 29
msg_12:
.ascii "The integer has been removed."
.long 25
msg_13:
.ascii "The integer is not found."
.long 31
msg_14:
.ascii "All integers have been removed."
.long 13
msg_15:
.ascii "Goodbye Human"
.long 23
msg_16:
.ascii "Error: unknown choice ("
.long 1
msg_17:
.ascii ")"
.long 0
msg_18:
.ascii ""
.long 43
msg_19:
.ascii "==========================================="
.long 10
msg_2:
.ascii "There are "
.long 43
msg_20:
.ascii "========== Hash Table Program ============="
.long 43
msg_21:
.ascii "==========================================="
.long 43
msg_22:
.ascii "=                                         ="
.long 43
msg_23:
.ascii "= Please choose the following options:    ="
.long 43
msg_24:
.ascii "=                                         ="
.long 43
msg_25:
.ascii "= a: insert an integer                    ="
.long 43
msg_26:
.ascii "= b: find an integer                      ="
.long 43
msg_27:
.ascii "= c: count the integers                   ="
.long 43
msg_28:
.ascii "= d: print all integers                   ="
.long 43
msg_29:
.ascii "= e: remove an integer                    ="
.long 10
msg_3:
.ascii " integers."
.long 43
msg_30:
.ascii "= f: remove all integers                  ="
.long 43
msg_31:
.ascii "= g: exit                                 ="
.long 43
msg_32:
.ascii "=                                         ="
.long 43
msg_33:
.ascii "==========================================="
.long 15
msg_34:
.ascii "Your decision: "
.long 18
msg_35:
.ascii "You have entered: "
.long 36
msg_36:
.ascii " which is invalid, please try again."
.long 33
msg_4:
.ascii "Please enter an integer to find: "
.long 17
msg_5:
.ascii "Find the integer."
.long 25
msg_6:
.ascii "The integer is not found."
.long 35
msg_7:
.ascii "Please enter an integer to insert: "
.long 43
msg_8:
.ascii "Successfully insert it. The integer is new."
.long 51
msg_9:
.ascii "The integer is already there. No insertion is made."
.section .text
.global askForInt
askForInt:
	PUSH {R4, LR}
	BL __print_string
	BL __read_int
	MOV R4, R0
	ADR R0, msg_0
	BL __print_ref
	MOV R0, R4
	BL __print_int
	BL __println
	MOV R0, R4
	POP {R4, PC}
.global calculateIndex
calculateIndex:
	PUSH {LR}
	MOV R0, R1
	LDR R1, [R0, #-1]
	BL __aeabi_idivmode
	MOV R0, R1
	POP {PC}
.global contain
contain:
	PUSH {R4, R5, LR}
	MOV R4, R0
	MOV R5, R1
	MOV R0, R4
	MOV R1, R5
	BL calculateIndex
	LDR R0, [R4, R0]
	MOV R1, R5
	BL findNode
	CMP R0, #0
	MOVNE R0, #1
	POP {R4, R5, PC}
.global count
count:
	PUSH {R4, R5, R6, R7, LR}
	MOV R5, R0
	LDR R6, [R5, #-1]
	MOV R7, #0
	MOV R4, #0
	CMP R4, R6
	BGTE L30
L23:
	LDR R0, [R5, R4]
	BL countNodes
	ADD R7, R7, R0
	ADD R4, R4, #1
	CMP R4, R6
	BLT L23
L30:
	MOV R0, R7
	POP {R4, R5, R6, R7, PC}
.global countNodes
countNodes:
	PUSH {LR}
	MOV R1, #0
	CMP R0, #0
	BEQ L39
L34:
	ADD R1, R1, #1
	LDR R0, [R0, #4]
	CMP R0, #0
	BNE L34
L39:
	MOV R0, R1
	POP {PC}
.global findNode
findNode:
	PUSH {LR}
	CMP R0, #0
	BEQ L49
L42:
	LDR R2, [R0, #0]
	CMP R2, R1
	BNE L46
	POP {PC}
	B L47
L46:
	LDR R0, [R0, #4]
L47:
	CMP R0, #0
	BNE L42
L49:
	MOV R0, #0
	POP {PC}
.global handleMenuCount
handleMenuCount:
	PUSH {R4, LR}
	BL count
	MOV R4, R0
	CMP R4, #1
	BNE L59
	ADR R0, msg_1
	BL __print_ref
	BL __println
	B L65
L59:
	ADR R0, msg_2
	BL __print_ref
	MOV R0, R4
	BL __print_int
	ADR R0, msg_3
	BL __print_ref
	BL __println
L65:
	MOV R0, #1
	POP {R4, PC}
.global handleMenuFind
handleMenuFind:
	PUSH {R4, LR}
	MOV R4, R0
	ADR R0, msg_4
	BL askForInt
	MOV R0, R4
	MOV R1, R0
	BL contain
	TST R0, R0
	BNE L77
	ADR R0, msg_5
	BL __print_ref
	BL __println
	B L80
L77:
	ADR R0, msg_6
	BL __print_ref
	BL __println
L80:
	MOV R0, #1
	POP {R4, PC}
.global handleMenuInsert
handleMenuInsert:
	PUSH {R4, LR}
	MOV R4, R0
	ADR R0, msg_7
	BL askForInt
	MOV R0, R4
	MOV R1, R0
	BL insertIfNotContain
	TST R0, R0
	BNE L92
	ADR R0, msg_8
	BL __print_ref
	BL __println
	B L95
L92:
	ADR R0, msg_9
	BL __print_ref
	BL __println
L95:
	MOV R0, #1
	POP {R4, PC}
.global handleMenuPrint
handleMenuPrint:
	PUSH {R4, LR}
	MOV R4, R0
	ADR R0, msg_10
	BL __print_ref
	MOV R0, R4
	BL printAll
	MOV R0, #1
	POP {R4, PC}
.global handleMenuRemove
handleMenuRemove:
	PUSH {R4, LR}
	MOV R4, R0
	ADR R0, msg_11
	BL askForInt
	MOV R0, R4
	MOV R1, R0
	BL remove
	TST R0, R0
	BNE L113
	ADR R0, msg_12
	BL __print_ref
	BL __println
	B L116
L113:
	ADR R0, msg_13
	BL __print_ref
	BL __println
L116:
	MOV R0, #1
	POP {R4, PC}
.global handleMenuRemoveAll
handleMenuRemoveAll:
	PUSH {LR}
	BL removeAll
	ADR R0, msg_14
	BL __print_ref
	BL __println
	MOV R0, #1
	POP {PC}
.global init
init:
	PUSH {LR}
	MOV R1, R0
	LDR R2, [R1, #-1]
	MOV R0, #0
	CMP R0, R2
	BGTE L133
L128:
	MOV R3, #0
	STR R3, [R1, R0]
	ADD R0, R0, #1
	CMP R0, R2
	BLT L128
L133:
	MOV R0, #1
	POP {PC}
.global insertIfNotContain
insertIfNotContain:
	PUSH {R4, R5, R6, R7, LR}
	MOV R4, R0
	MOV R5, R1
	MOV R0, R4
	MOV R1, R5
	BL calculateIndex
	MOV R6, R0
	LDR R0, [R4, R6]
	MOV R1, R5
	BL findNode
	CMP R0, #0
	BEQ L145
	MOV R0, #0
	POP {R4, R5, R6, R7, PC}
	B L152
L145:
	LDR R7, [R4, R6]
	BL __alloc
	STR R5, [R0, #0]
	STR R7, [R0, #4]
	STR R0, [R4, R6]
	MOV R0, #1
	POP {R4, R5, R6, R7, PC}
.global main
main:
	PUSH {R4, R5, R6, R7, R8, R9, LR}
	SUB SP, SP, #20
	BL __alloc
	MOV R4, R0
	MOV R0, #0
	MOV R1, #0
	MOV R2, #0
	MOV R3, #0
	MOV R5, #0
	MOV R6, #0
	MOV R7, #0
	MOV R8, #0
	MOV R9, #0
	MOV R12, #0
	STR R12, [SP, #0]
	MOV R12, #0
	STR R12, [SP, #-4]
	MOV R12, #0
	STR R12, [SP, #-8]
	MOV R12, #0
	STR R12, [SP, #-12]
	STR R0, [R4, #0]
	STR R1, [R4, #1]
	STR R2, [R4, #2]
	STR R3, [R4, #3]
	STR R5, [R4, #4]
	STR R6, [R4, #5]
	STR R7, [R4, #6]
	STR R8, [R4, #7]
	STR R9, [R4, #8]
	LDR R11, [SP, #0]
	STR R11, [R4, #9]
	LDR R11, [SP, #-4]
	STR R11, [R4, #10]
	LDR R11, [SP, #-8]
	STR R11, [R4, #11]
	LDR R11, [SP, #-12]
	STR R11, [R4, #12]
	MOV R0, R4
	BL init
	MOV R0, #1
	TST R0, R0
	BNE L245
L196:
	BL printMenu
	MOV R5, R0
	MOV R0, #97
	CMP R5, R0
	BNE L203
	MOV R0, R4
	BL handleMenuInsert
	B L244
L203:
	MOV R0, #98
	CMP R5, R0
	BNE L208
	MOV R0, R4
	BL handleMenuFind
	B L244
L208:
	MOV R0, #99
	CMP R5, R0
	BNE L213
	MOV R0, R4
	BL handleMenuCount
	B L244
L213:
	MOV R0, #100
	CMP R5, R0
	BNE L218
	MOV R0, R4
	BL handleMenuPrint
	B L244
L218:
	MOV R0, #101
	CMP R5, R0
	BNE L223
	MOV R0, R4
	BL handleMenuRemove
	B L244
L223:
	MOV R0, #102
	CMP R5, R0
	BNE L228
	MOV R0, R4
	BL handleMenuRemoveAll
	B L244
L228:
	MOV R0, #103
	CMP R5, R0
	BNE L235
	ADR R0, msg_15
	BL __print_ref
	BL __println
	MOV R12, #0
	STR R12, [SP, #-16]
	B L244
L235:
	ADR R0, msg_16
	BL __print_ref
	MOV R0, R5
	BL __print_char
	ADR R0, msg_17
	BL __print_ref
	BL __println
	MOV R0, #1
	NEG R0, R0
	BL __exit
L244:
	LDR R12, [SP, #-16]
	TST R12, R12
	BNE L196
L245:
	ADD SP, SP, #20
	MOV R0, #0
	POP {R4, R5, R6, R7, R8, R9, PC}
.global printAll
printAll:
	PUSH {R4, R5, R6, LR}
	MOV R5, R0
	LDR R6, [R5, #-1]
	MOV R4, #0
	CMP R4, R6
	BGTE L256
L250:
	LDR R0, [R5, R4]
	BL printAllNodes
	ADD R4, R4, #1
	CMP R4, R6
	BLT L250
L256:
	ADR R0, msg_18
	BL __print_ref
	BL __println
	MOV R0, #1
	POP {R4, R5, R6, PC}
.global printAllNodes
printAllNodes:
	PUSH {R4, LR}
	CMP R4, #0
	BEQ L270
L263:
	LDR R0, [R4, #0]
	BL __print_int
	MOV R0, #32
	BL __print_char
	LDR R4, [R4, #4]
	CMP R4, #0
	BNE L263
L270:
	MOV R0, #1
	POP {R4, PC}
.global printMenu
printMenu:
	PUSH {R4, R5, R6, LR}
	ADR R0, msg_19
	BL __print_ref
	BL __println
	ADR R0, msg_20
	BL __print_ref
	BL __println
	ADR R0, msg_21
	BL __print_ref
	BL __println
	ADR R0, msg_22
	BL __print_ref
	BL __println
	ADR R0, msg_23
	BL __print_ref
	BL __println
	ADR R0, msg_24
	BL __print_ref
	BL __println
	ADR R0, msg_25
	BL __print_ref
	BL __println
	ADR R0, msg_26
	BL __print_ref
	BL __println
	ADR R0, msg_27
	BL __print_ref
	BL __println
	ADR R0, msg_28
	BL __print_ref
	BL __println
	ADR R0, msg_29
	BL __print_ref
	BL __println
	ADR R0, msg_30
	BL __print_ref
	BL __println
	ADR R0, msg_31
	BL __print_ref
	BL __println
	ADR R0, msg_32
	BL __print_ref
	BL __println
	ADR R0, msg_33
	BL __print_ref
	BL __println
	MOV R0, #97
	MOV R5, R0
	MOV R0, #103
	MOV R6, R0
	MOV R0, #1
	TST R0, R0
	BNE L340
L323:
	ADR R0, msg_34
	BL __print_ref
	BL __read_char
	MOV R4, R0
	MOV R0, R4
	CMP R5, R0
	BGT L332
	CMP R0, R6
	BGT L332
	MOV R0, R4
	POP {R4, R5, R6, PC}
	B L338
L332:
	ADR R0, msg_35
	BL __print_ref
	MOV R0, R4
	BL __print_char
	ADR R0, msg_36
	BL __print_ref
	BL __println
L338:
	MOV R0, #1
	TST R0, R0
	BNE L323
L340:
	MOV R0, #0
	POP {R4, R5, R6, PC}
.global remove
remove:
	PUSH {R4, R5, R6, LR}
	MOV R4, R0
	MOV R6, R1
	MOV R0, R4
	MOV R1, R6
	BL calculateIndex
	MOV R5, R0
	LDR R0, [R4, R5]
	MOV R1, R6
	BL findNode
	CMP R0, #0
	BNE L352
	MOV R0, #0
	POP {R4, R5, R6, PC}
	B L357
L352:
	LDR R1, [R4, R5]
	MOV R0, R1
	MOV R1, R0
	BL removeNode
	STR R0, [R4, R5]
	MOV R0, #1
	POP {R4, R5, R6, PC}
.global removeAll
removeAll:
	PUSH {R4, R5, R6, R7, R8, LR}
	MOV R5, R0
	LDR R7, [R5, #-1]
	MOV R4, #0
	CMP R4, R7
	BGTE L373
L360:
	LDR R6, [R5, R4]
	CMP R6, #0
	BEQ L368
L363:
	LDR R8, [R6, #4]
	MOV R0, R6
	BL __free
	MOV R0, R8
	CMP R0, #0
	BNE L363
L368:
	MOV R0, #0
	STR R0, [R5, R4]
	ADD R4, R4, #1
	CMP R4, R7
	BLT L360
L373:
	MOV R0, #1
	POP {R4, R5, R6, R7, R8, PC}
.global removeNode
removeNode:
	PUSH {R4, R5, LR}
	MOV R5, R1
	CMP R0, #0
	BNE L380
	MOV R0, #0
	POP {R4, R5, PC}
	B L389
L380:
	CMP R0, R5
	BNE L385
	LDR R4, [R0, #4]
	MOV R0, R5
	BL __free
	MOV R0, R4
	POP {R4, R5, PC}
	B L389
L385:
	LDR R0, [R4, #4]
	MOV R1, R5
	BL removeNode
	STR R0, [R4, #4]
	MOV R0, R4
	POP {R4, R5, PC}