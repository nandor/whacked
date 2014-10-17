	.cpu arm7tdmi
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 1
	.eabi_attribute 30, 2
	.eabi_attribute 34, 0
	.eabi_attribute 18, 4
	.file	"test.c"
	.text
	.align	2
	.global	test
	.type	test, %function
test:
	@ Function supports interworking.
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	add	r0, r0, #2
	add	r0, r0, r1
	bx	lr
	.size	test, .-test
	.global	__aeabi_idiv
	.section	.text.startup,"ax",%progbits
	.align	2
	.global	main
	.type	main, %function
main:
	@ Function supports interworking.
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	str	lr, [sp, #-4]!
	sub	sp, sp, #12
	ldr	r3, [sp, #4]
.L3:
	cmp	r3, #0
	blt	.L6
.L4:
	b	.L4
.L6:
	ldr	r0, .L11
	add	r1, sp, #4
	bl	scanf
	ldr	r3, [sp, #4]
	cmp	r3, #0
	blt	.L3
	ldr	r1, [sp, #4]
	ldr	r0, .L11+4
	bl	__aeabi_idiv
	mov	r1, r0
	ldr	r0, .L11
	bl	printf
	add	sp, sp, #12
	@ sp needed
	ldr	lr, [sp], #4
	bx	lr
.L12:
	.align	2
.L11:
	.word	.LC0
	.word	12345678
	.size	main, .-main
	.section	.rodata.str1.4,"aMS",%progbits,1
	.align	2
.LC0:
	.ascii	"%d\012\000"
	.ident	"GCC: (Arch Repository) 4.9.1"
