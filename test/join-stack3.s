.data
.align	8
.text
f.10:
	movl	$123, %eax
	ret
g.12:
	movl	$456, %eax
	ret
h.14:
	movl	$789, %eax
	ret
.global	min_caml_start
min_caml_start:
.global	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	call	f.10
	movl	%eax, 0(%ebp)
	cmpl	$0, %eax
	jg	jle_else.27
	addl	$8, %ebp
	call	g.12
	subl	$8, %ebp
	jmp	jle_cont.28
jle_else.27:
	addl	$8, %ebp
	call	h.14
	subl	$8, %ebp
jle_cont.28:
	movl	0(%ebp), %ebx
	addl	%ebx, %eax
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
