	.text
	.file	"main.7rcbfp3g-cgu.0"
	.section	".text._ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E,@function
_ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	_ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E, .Lfunc_end0-_ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E
	.cfi_endproc

	.section	".text._ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E,@function
_ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movq	%rdi, 48(%rsp)
	movq	%rsi, %rdi
	movq	%rsi, 40(%rsp)
	movq	%rdx, %rsi
	movq	%rdx, 32(%rsp)
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 24(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	24(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	movq	56(%rsp), %rdi
	movq	64(%rsp), %rsi
	movq	40(%rsp), %rdx
	movq	32(%rsp), %rcx
	callq	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	_ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E, .Lfunc_end1-_ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E
	.cfi_endproc

	.section	".text._ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE,@function
_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE:
	.cfi_startproc
	movq	16(%rdi), %rax
	retq
.Lfunc_end2:
	.size	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE, .Lfunc_end2-_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE
	.cfi_endproc

	.section	".text._ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E,@function
_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E:
.Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception0
	subq	$296, %rsp
	.cfi_def_cfa_offset 304
	movq	%rdi, %rax
	movq	%rdx, 224(%rsp)
.Ltmp0:
	movq	%rdi, 216(%rsp)
	movq	%rsi, %rdi
	movq	%rax, 208(%rsp)
	movq	%rsi, 200(%rsp)
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE
.Ltmp1:
	movq	%rax, 192(%rsp)
	jmp	.LBB3_2
.LBB3_1:
	movq	280(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB3_2:
.Ltmp2:
	leaq	224(%rsp), %rdi
	callq	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE
.Ltmp3:
	movq	%rdx, 184(%rsp)
	movq	%rax, 176(%rsp)
	jmp	.LBB3_4
.LBB3_3:
	jmp	.LBB3_1
.LBB3_4:
	movq	176(%rsp), %rax
	movq	%rax, 240(%rsp)
	movq	184(%rsp), %rcx
	movq	%rcx, 248(%rsp)
	movq	240(%rsp), %rdx
	testq	%rdx, %rdx
	movq	%rdx, 168(%rsp)
	je	.LBB3_7
	jmp	.LBB3_29
.LBB3_29:
	movq	168(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 160(%rsp)
	je	.LBB3_8
	jmp	.LBB3_30
.LBB3_30:
	movq	168(%rsp), %rax
	subq	$2, %rax
	movq	%rax, 152(%rsp)
	jne	.LBB3_6
	jmp	.LBB3_5
.LBB3_5:
	movq	$0, 232(%rsp)
	jmp	.LBB3_9
.LBB3_6:
	ud2
.LBB3_7:
	movq	248(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 232(%rsp)
	jmp	.LBB3_9
.LBB3_8:
	movq	248(%rsp), %rax
	movq	(%rax), %rax
	addq	$1, %rax
	movq	%rax, 232(%rsp)
.LBB3_9:
.Ltmp4:
	leaq	224(%rsp), %rdi
	callq	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE
.Ltmp5:
	movq	%rdx, 144(%rsp)
	movq	%rax, 136(%rsp)
	jmp	.LBB3_10
.LBB3_10:
	movq	136(%rsp), %rax
	movq	%rax, 264(%rsp)
	movq	144(%rsp), %rcx
	movq	%rcx, 272(%rsp)
	movq	264(%rsp), %rdx
	testq	%rdx, %rdx
	movq	%rdx, 128(%rsp)
	je	.LBB3_12
	jmp	.LBB3_31
.LBB3_31:
	movq	128(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 120(%rsp)
	je	.LBB3_13
	jmp	.LBB3_32
.LBB3_32:
	movq	128(%rsp), %rax
	subq	$2, %rax
	movq	%rax, 112(%rsp)
	jne	.LBB3_6
	jmp	.LBB3_11
.LBB3_11:
	movq	192(%rsp), %rax
	movq	%rax, 256(%rsp)
	jmp	.LBB3_14
.LBB3_12:
	movq	272(%rsp), %rax
	movq	(%rax), %rax
	addq	$1, %rax
	movq	%rax, 256(%rsp)
	jmp	.LBB3_14
.LBB3_13:
	movq	272(%rsp), %rax
	movq	(%rax), %rax
	movq	%rax, 256(%rsp)
.LBB3_14:
	movq	232(%rsp), %rax
	cmpq	256(%rsp), %rax
	setbe	%cl
	xorb	$-1, %cl
	testb	$1, %cl
	jne	.LBB3_15
	jmp	.LBB3_16
.LBB3_15:
.Ltmp22:
	leaq	.L__unnamed_1(%rip), %rdi
	movq	_ZN4core9panicking5panic17hb30cf43f16b71b76E@GOTPCREL(%rip), %rax
	callq	*%rax
.Ltmp23:
	jmp	.LBB3_28
.LBB3_16:
	movq	192(%rsp), %rax
	cmpq	%rax, 256(%rsp)
	setbe	%cl
	xorb	$-1, %cl
	testb	$1, %cl
	jne	.LBB3_17
	jmp	.LBB3_18
.LBB3_17:
.Ltmp20:
	leaq	.L__unnamed_2(%rip), %rdi
	movq	_ZN4core9panicking5panic17hb30cf43f16b71b76E@GOTPCREL(%rip), %rax
	callq	*%rax
.Ltmp21:
	jmp	.LBB3_28
.LBB3_18:
	movq	232(%rsp), %rsi
.Ltmp6:
	movq	200(%rsp), %rdi
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E
.Ltmp7:
	jmp	.LBB3_19
.LBB3_19:
.Ltmp8:
	movq	200(%rsp), %rdi
	callq	_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE
.Ltmp9:
	movq	%rdx, 104(%rsp)
	movq	%rax, 96(%rsp)
	jmp	.LBB3_20
.LBB3_20:
.Ltmp10:
	movq	96(%rsp), %rdi
	movq	104(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE
.Ltmp11:
	movq	%rax, 88(%rsp)
	jmp	.LBB3_21
.LBB3_21:
	movq	232(%rsp), %rsi
.Ltmp12:
	movq	88(%rsp), %rdi
	callq	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE
.Ltmp13:
	movq	%rax, 80(%rsp)
	jmp	.LBB3_22
.LBB3_22:
	movq	256(%rsp), %rax
	movq	232(%rsp), %rcx
	subq	%rcx, %rax
.Ltmp14:
	movq	80(%rsp), %rdi
	movq	%rax, %rsi
	callq	_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE
.Ltmp15:
	movq	%rdx, 72(%rsp)
	movq	%rax, 64(%rsp)
	jmp	.LBB3_23
.LBB3_23:
	movq	256(%rsp), %rax
	movq	192(%rsp), %rcx
	subq	%rax, %rcx
.Ltmp16:
	movq	64(%rsp), %rdi
	movq	72(%rsp), %rsi
	movq	%rax, 56(%rsp)
	movq	%rcx, 48(%rsp)
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE
.Ltmp17:
	movq	%rdx, 40(%rsp)
	movq	%rax, 32(%rsp)
	jmp	.LBB3_24
.LBB3_24:
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
.Ltmp18:
	movq	200(%rsp), %rdi
	movq	%rax, 24(%rsp)
	movq	%rcx, 16(%rsp)
	callq	_ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E
.Ltmp19:
	movq	%rax, 8(%rsp)
	jmp	.LBB3_25
.LBB3_25:
	movq	216(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	%rcx, (%rax)
	movq	48(%rsp), %rdx
	movq	%rdx, 8(%rax)
	movq	24(%rsp), %rsi
	movq	%rsi, 16(%rax)
	movq	16(%rsp), %rdi
	movq	%rdi, 24(%rax)
	movq	8(%rsp), %r8
	movq	%r8, 32(%rax)
	movq	208(%rsp), %rax
	addq	$296, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB3_27:
	.cfi_def_cfa_offset 304
.Ltmp24:
	movl	%edx, %ecx
	movq	%rax, 280(%rsp)
	movl	%ecx, 288(%rsp)
	jmp	.LBB3_3
.LBB3_28:
	ud2
.Lfunc_end3:
	.size	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E, .Lfunc_end3-_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table3:
.Lexception0:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end0-.Lcst_begin0
.Lcst_begin0:
	.uleb128 .Ltmp0-.Lfunc_begin0
	.uleb128 .Ltmp1-.Ltmp0
	.uleb128 .Ltmp24-.Lfunc_begin0
	.byte	0
	.uleb128 .Ltmp1-.Lfunc_begin0
	.uleb128 .Ltmp2-.Ltmp1
	.byte	0
	.byte	0
	.uleb128 .Ltmp2-.Lfunc_begin0
	.uleb128 .Ltmp19-.Ltmp2
	.uleb128 .Ltmp24-.Lfunc_begin0
	.byte	0
.Lcst_end0:
	.p2align	2

	.section	".text._ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E,@function
_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E:
	.cfi_startproc
	movq	%rsi, 16(%rdi)
	retq
.Lfunc_end4:
	.size	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E, .Lfunc_end4-_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E
	.cfi_endproc

	.section	".text._ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E,@function
_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end5:
	.size	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E, .Lfunc_end5-_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E
	.cfi_endproc

	.section	".text._ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E,@function
_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E:
	.cfi_startproc
	subq	$88, %rsp
	.cfi_def_cfa_offset 96
	movq	%rdi, 64(%rsp)
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E
	movq	%rax, 56(%rsp)
	movq	56(%rsp), %rax
	addq	$16, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE
	movq	64(%rsp), %rdi
	callq	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E
	movq	%rax, 48(%rsp)
	movl	$1, %eax
	movl	%eax, %esi
	movq	48(%rsp), %rcx
	addq	$8, %rcx
	movb	$1, 78(%rsp)
	movq	%rcx, %rdi
	movzbl	78(%rsp), %edx
	callq	_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E
	movq	%rax, 40(%rsp)
	movq	40(%rsp), %rax
	cmpq	$1, %rax
	jne	.LBB6_11
	movb	$2, 79(%rsp)
	movzbl	79(%rsp), %edi
	callq	_ZN4core4sync6atomic5fence17h20ad0752fee06055E
	movq	64(%rsp), %rax
	movq	(%rax), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE
	movq	%rax, 32(%rsp)
	movq	64(%rsp), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E
	movq	%rax, 24(%rsp)
	movq	24(%rsp), %rdi
	callq	_ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	leaq	80(%rsp), %rdi
	movq	32(%rsp), %rsi
	movq	16(%rsp), %rdx
	movq	8(%rsp), %rcx
	callq	_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E
	jmp	.LBB6_11
.LBB6_11:
	addq	$88, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end6:
	.size	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E, .Lfunc_end6-_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E
	.cfi_endproc

	.section	".text._ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E,@function
_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E:
	.cfi_startproc
	movq	%rdi, %rax
	movq	%rsi, %rdx
	retq
.Lfunc_end7:
	.size	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E, .Lfunc_end7-_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E
	.cfi_endproc

	.section	".text._ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE,@function
_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end8:
	.size	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE, .Lfunc_end8-_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE
	.cfi_endproc

	.section	".text._ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E,@function
_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end9:
	.size	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E, .Lfunc_end9-_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E
	.cfi_endproc

	.section	".text._ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E,@function
_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end10:
	.size	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E, .Lfunc_end10-_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rdi
	movq	%rdi, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end11:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E, .Lfunc_end11-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E
	movq	%rax, 16(%rsp)
	movq	16(%rsp), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end12:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E, .Lfunc_end12-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E
	movq	%rax, 16(%rsp)
	movq	16(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$13new_unchecked17he24ff91371a6c3e1E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end13:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE, .Lfunc_end13-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hc5aec9c6fb51436aE
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end14:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E, .Lfunc_end14-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17h2c2fac8613781369E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end15:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E, .Lfunc_end15-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end16:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E, .Lfunc_end16-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end17:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E, .Lfunc_end17-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end18:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE, .Lfunc_end18-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hf8e833184d0b029eE
	.cfi_endproc

	.section	".text._ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E,@function
_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17h03bcb7648f8c0f84E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end19:
	.size	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E, .Lfunc_end19-_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ref17h065513df9d061587E
	.cfi_endproc

	.section	".text._ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E,@function
_ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E:
.Lfunc_begin1:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception1
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movq	%rdi, 24(%rsp)
	movb	$0, 54(%rsp)
	movb	$0, 55(%rsp)
	movb	$1, 54(%rsp)
	movb	$1, 55(%rsp)
	movq	24(%rsp), %rdi
	testq	%rdi, %rdi
	setne	%al
	movzbl	%al, %ecx
	movl	%ecx, %edi
	movq	%rdi, 16(%rsp)
	je	.LBB20_2
	jmp	.LBB20_18
.LBB20_18:
	movq	16(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 8(%rsp)
	je	.LBB20_4
	jmp	.LBB20_3
.LBB20_1:
	movq	56(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB20_2:
	movb	$0, 32(%rsp)
	jmp	.LBB20_6
.LBB20_3:
	ud2
.LBB20_4:
	movb	$0, 54(%rsp)
	movq	24(%rsp), %rax
	movb	$0, 55(%rsp)
	movq	%rax, 40(%rsp)
	movq	40(%rsp), %rdi
.Ltmp25:
	callq	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE
.Ltmp26:
	movb	%al, 7(%rsp)
	jmp	.LBB20_5
.LBB20_5:
	movb	7(%rsp), %al
	movb	%al, 33(%rsp)
	movb	$1, 32(%rsp)
.LBB20_6:
	testb	$1, 55(%rsp)
	jne	.LBB20_12
.LBB20_7:
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	cmpq	$0, 24(%rsp)
	cmoveq	%rdx, %rcx
	cmpq	$1, %rcx
	je	.LBB20_14
	jmp	.LBB20_16
.LBB20_8:
	testb	$1, 54(%rsp)
	je	.LBB20_1
	movb	$0, 54(%rsp)
	jmp	.LBB20_1
.LBB20_10:
	jmp	.LBB20_1
.LBB20_11:
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	cmpq	$0, 24(%rsp)
	cmoveq	%rdx, %rcx
	cmpq	$1, %rcx
	je	.LBB20_8
	jmp	.LBB20_10
.LBB20_12:
	movb	$0, 55(%rsp)
	jmp	.LBB20_7
.LBB20_13:
	movb	32(%rsp), %al
	movb	33(%rsp), %dl
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB20_14:
	.cfi_def_cfa_offset 80
	testb	$1, 54(%rsp)
	je	.LBB20_13
	movb	$0, 54(%rsp)
	jmp	.LBB20_13
.LBB20_16:
	jmp	.LBB20_13
.LBB20_17:
.Ltmp27:
	movl	%edx, %ecx
	movq	%rax, 56(%rsp)
	movl	%ecx, 64(%rsp)
	jmp	.LBB20_11
.Lfunc_end20:
	.size	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E, .Lfunc_end20-_ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table20:
.Lexception1:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end1-.Lcst_begin1
.Lcst_begin1:
	.uleb128 .Lfunc_begin1-.Lfunc_begin1
	.uleb128 .Ltmp25-.Lfunc_begin1
	.byte	0
	.byte	0
	.uleb128 .Ltmp25-.Lfunc_begin1
	.uleb128 .Ltmp26-.Ltmp25
	.uleb128 .Ltmp27-.Lfunc_begin1
	.byte	0
.Lcst_end1:
	.p2align	2

	.section	".text._ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE,@function
_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movb	(%rdi), %al
	subb	$2, %al
	setne	%cl
	movzbl	%cl, %edx
	movl	%edx, %esi
	movq	%rdi, 24(%rsp)
	movb	%al, 23(%rsp)
	movq	%rsi, 8(%rsp)
	je	.LBB21_1
	jmp	.LBB21_5
.LBB21_5:
	movq	8(%rsp), %rax
	subq	$1, %rax
	movq	%rax, (%rsp)
	je	.LBB21_3
	jmp	.LBB21_2
.LBB21_1:
	movq	$0, 32(%rsp)
	jmp	.LBB21_4
.LBB21_2:
	ud2
.LBB21_3:
	movq	24(%rsp), %rax
	movq	%rax, 32(%rsp)
.LBB21_4:
	movq	32(%rsp), %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end21:
	.size	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE, .Lfunc_end21-_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE
	.cfi_endproc

	.section	".text._ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E,@function
_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E:
.Lfunc_begin2:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception2
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 32(%rsp)
	movq	32(%rsp), %rdi
	testq	%rdi, %rdi
	setne	%al
	movzbl	%al, %ecx
	movl	%ecx, %edi
	movq	%rdi, 24(%rsp)
	je	.LBB22_2
	jmp	.LBB22_10
.LBB22_10:
	movq	24(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 16(%rsp)
	je	.LBB22_4
	jmp	.LBB22_3
.LBB22_1:
	movq	40(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB22_2:
.Ltmp28:
	leaq	.L__unnamed_3(%rip), %rdi
	movq	_ZN4core9panicking5panic17hb30cf43f16b71b76E@GOTPCREL(%rip), %rax
	callq	*%rax
.Ltmp29:
	jmp	.LBB22_8
.LBB22_3:
	ud2
.LBB22_4:
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	movq	32(%rsp), %rax
	cmpq	$0, 32(%rsp)
	cmoveq	%rdx, %rcx
	cmpq	$1, %rcx
	movq	%rax, 8(%rsp)
	je	.LBB22_6
	jmp	.LBB22_7
.LBB22_5:
	jmp	.LBB22_1
.LBB22_6:
	movq	8(%rsp), %rax
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB22_7:
	.cfi_def_cfa_offset 64
	jmp	.LBB22_6
.LBB22_8:
	ud2
.LBB22_9:
.Ltmp30:
	movl	%edx, %ecx
	movq	%rax, 40(%rsp)
	movl	%ecx, 48(%rsp)
	jmp	.LBB22_5
.Lfunc_end22:
	.size	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E, .Lfunc_end22-_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table22:
.Lexception2:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end2-.Lcst_begin2
.Lcst_begin2:
	.uleb128 .Lfunc_begin2-.Lfunc_begin2
	.uleb128 .Ltmp28-.Lfunc_begin2
	.byte	0
	.byte	0
	.uleb128 .Ltmp28-.Lfunc_begin2
	.uleb128 .Ltmp29-.Ltmp28
	.uleb128 .Ltmp30-.Lfunc_begin2
	.byte	0
.Lcst_end2:
	.p2align	2

	.section	".text._ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E,@function
_ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movb	(%rdi), %al
	subb	$2, %al
	setne	%cl
	movzbl	%cl, %edx
	movl	%edx, %edi
	movb	%al, 22(%rsp)
	movq	%rdi, 8(%rsp)
	je	.LBB23_2
	jmp	.LBB23_5
.LBB23_5:
	movq	8(%rsp), %rax
	subq	$1, %rax
	movq	%rax, (%rsp)
	jne	.LBB23_3
	jmp	.LBB23_1
.LBB23_1:
	movb	$1, 23(%rsp)
	jmp	.LBB23_4
.LBB23_2:
	movb	$0, 23(%rsp)
	jmp	.LBB23_4
.LBB23_3:
	ud2
.LBB23_4:
	movb	23(%rsp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end23:
	.size	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E, .Lfunc_end23-_ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E
	.cfi_endproc

	.section	.text._ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE,@function
_ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movb	%sil, %al
	movq	%rdi, %r8
	movq	%rdi, 32(%rsp)
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	movq	%r8, 24(%rsp)
	movb	%al, 23(%rsp)
	callq	_ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	32(%rsp), %rdi
	movb	23(%rsp), %al
	movzbl	%al, %esi
	movq	8(%rsp), %rdx
	movq	(%rsp), %rcx
	callq	*_ZN3std2io5error5Error4_new17hce901c8b61176a6dE@GOTPCREL(%rip)
	movq	24(%rsp), %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end24:
	.size	_ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE, .Lfunc_end24-_ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE
	.cfi_endproc

	.section	.text._ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE,@function
_ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE:
.Lfunc_begin3:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception3
	subq	$104, %rsp
	.cfi_def_cfa_offset 112
	movq	%rdi, %rax
	movb	$0, 86(%rsp)
	movb	$0, 87(%rsp)
	movb	$1, 86(%rsp)
	movb	$1, 87(%rsp)
	cmpq	$1, (%rsi)
	movq	%rax, 40(%rsp)
	movq	%rdx, 32(%rsp)
	movq	%rsi, 24(%rsp)
	movq	%rdi, 16(%rsp)
	je	.LBB25_2
	jmp	.LBB25_7
.LBB25_1:
	movq	88(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB25_2:
	movq	24(%rsp), %rax
	addq	$8, %rax
	movq	%rax, 48(%rsp)
	movq	48(%rsp), %rdi
.Ltmp31:
	movq	_ZN3std3sys4unix5stdio8is_ebadf17hf8fc2cf98cbb338aE@GOTPCREL(%rip), %rax
	callq	*%rax
.Ltmp32:
	movb	%al, 15(%rsp)
	jmp	.LBB25_3
.LBB25_3:
	movb	15(%rsp), %al
	testb	$1, %al
	jne	.LBB25_6
	jmp	.LBB25_7
.LBB25_4:
	testb	$1, 86(%rsp)
	jne	.LBB25_11
	jmp	.LBB25_1
.LBB25_5:
	movb	$0, 87(%rsp)
	jmp	.LBB25_4
.LBB25_6:
	movb	$0, 87(%rsp)
	movq	16(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	jmp	.LBB25_8
.LBB25_7:
	movb	$0, 86(%rsp)
	movq	24(%rsp), %rax
	movq	(%rax), %rcx
	movq	%rcx, 56(%rsp)
	movq	8(%rax), %rcx
	movq	%rcx, 64(%rsp)
	movq	16(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	56(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rcx, (%rdx)
	movq	64(%rsp), %rcx
	movq	%rcx, 8(%rdx)
	movq	72(%rsp), %rcx
	movq	%rcx, 16(%rdx)
.LBB25_8:
	testb	$1, 87(%rsp)
	jne	.LBB25_12
.LBB25_9:
	testb	$1, 86(%rsp)
	jne	.LBB25_13
.LBB25_10:
	movq	40(%rsp), %rax
	addq	$104, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB25_11:
	.cfi_def_cfa_offset 112
	movb	$0, 86(%rsp)
	movq	24(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	jmp	.LBB25_1
.LBB25_12:
	movb	$0, 87(%rsp)
	jmp	.LBB25_9
.LBB25_13:
	movb	$0, 86(%rsp)
	movq	24(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	jmp	.LBB25_10
.LBB25_14:
.Ltmp33:
	movl	%edx, %ecx
	movq	%rax, 88(%rsp)
	movl	%ecx, 96(%rsp)
	jmp	.LBB25_5
.Lfunc_end25:
	.size	_ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE, .Lfunc_end25-_ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table25:
.Lexception3:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end3-.Lcst_begin3
.Lcst_begin3:
	.uleb128 .Lfunc_begin3-.Lfunc_begin3
	.uleb128 .Ltmp31-.Lfunc_begin3
	.byte	0
	.byte	0
	.uleb128 .Ltmp31-.Lfunc_begin3
	.uleb128 .Ltmp32-.Ltmp31
	.uleb128 .Ltmp33-.Lfunc_begin3
	.byte	0
	.uleb128 .Ltmp32-.Lfunc_begin3
	.uleb128 .Lfunc_end25-.Ltmp32
	.byte	0
	.byte	0
.Lcst_end3:
	.p2align	2

	.section	.text._ZN3std2rt10lang_start17hf4574541ae2d563fE,"ax",@progbits
	.hidden	_ZN3std2rt10lang_start17hf4574541ae2d563fE
	.globl	_ZN3std2rt10lang_start17hf4574541ae2d563fE
	.p2align	4, 0x90
	.type	_ZN3std2rt10lang_start17hf4574541ae2d563fE,@function
_ZN3std2rt10lang_start17hf4574541ae2d563fE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	leaq	.L__unnamed_4(%rip), %rax
	movq	%rdi, 32(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rsi, 24(%rsp)
	movq	%rax, %rsi
	movq	24(%rsp), %rax
	movq	%rdx, 16(%rsp)
	movq	%rax, %rdx
	movq	16(%rsp), %rcx
	callq	*_ZN3std2rt19lang_start_internal17h15a89b80169b8a87E@GOTPCREL(%rip)
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end26:
	.size	_ZN3std2rt10lang_start17hf4574541ae2d563fE, .Lfunc_end26-_ZN3std2rt10lang_start17hf4574541ae2d563fE
	.cfi_endproc

	.section	".text._ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E,@function
_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	*(%rdi)
	callq	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE
	movl	%eax, 4(%rsp)
	movl	4(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end27:
	.size	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E, .Lfunc_end27-_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E
	.cfi_endproc

	.section	.text._ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE,@function
_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE:
	.cfi_startproc
	movzbl	(%rdi), %eax
	retq
.Lfunc_end28:
	.size	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE, .Lfunc_end28-_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE
	.cfi_endproc

	.section	".text._ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E,@function
_ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end29:
	.size	_ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E, .Lfunc_end29-_ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E
	.cfi_endproc

	.section	".text._ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE,@function
_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rax
	movq	8(%rdi), %rdi
	movq	%rdi, 16(%rsp)
	movq	%rax, %rdi
	movq	16(%rsp), %rax
	movq	%rsi, 8(%rsp)
	movq	%rax, %rsi
	movq	8(%rsp), %rdx
	callq	*_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h0542e725e0a50084E@GOTPCREL(%rip)
	movb	%al, 7(%rsp)
	movb	7(%rsp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end30:
	.size	_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE, .Lfunc_end30-_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE
	.cfi_endproc

	.section	".text._ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E,@function
_ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E:
.Lfunc_begin4:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception4
	subq	$344, %rsp
	.cfi_def_cfa_offset 352
	movq	%rdi, %rax
	movb	$0, 327(%rsp)
	movq	$0, 88(%rsp)
	movq	%rsi, %rcx
	movq	%rdi, 80(%rsp)
	movq	%rcx, %rdi
	movq	%rax, 72(%rsp)
	movq	%rsi, 64(%rsp)
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE
	movq	%rax, 56(%rsp)
	jmp	.LBB31_2
.LBB31_1:
	movq	328(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB31_2:
	movb	$3, 96(%rsp)
.LBB31_3:
	movq	56(%rsp), %rax
	cmpq	%rax, 88(%rsp)
	jb	.LBB31_5
.LBB31_4:
	cmpq	$0, 88(%rsp)
	ja	.LBB31_25
	jmp	.LBB31_28
.LBB31_5:
	movq	64(%rsp), %rax
	movb	$1, 25(%rax)
	addq	$24, %rax
.Ltmp34:
	movq	%rax, %rdi
	callq	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6as_mut17h49480149a2e6362cE
.Ltmp35:
	movq	%rax, 48(%rsp)
	jmp	.LBB31_6
.LBB31_6:
.Ltmp36:
	movq	48(%rsp), %rdi
	callq	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap17hbd81e9bb5eefc001E
.Ltmp37:
	movq	%rax, 40(%rsp)
	jmp	.LBB31_8
.LBB31_7:
	leaq	96(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
	jmp	.LBB31_1
.LBB31_8:
	movq	88(%rsp), %rax
	movq	%rax, 144(%rsp)
	movq	144(%rsp), %rsi
.Ltmp38:
	movq	64(%rsp), %rdi
	callq	_ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E
.Ltmp39:
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
	jmp	.LBB31_9
.LBB31_9:
.Ltmp40:
	leaq	120(%rsp), %rdi
	movq	40(%rsp), %rsi
	movq	24(%rsp), %rdx
	movq	32(%rsp), %rcx
	callq	_ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE
.Ltmp41:
	jmp	.LBB31_10
.LBB31_10:
	movb	$1, 327(%rsp)
	movq	64(%rsp), %rax
	movb	$0, 25(%rax)
	movq	120(%rsp), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 16(%rsp)
	je	.LBB31_14
	jmp	.LBB31_44
.LBB31_44:
	movq	16(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 8(%rsp)
	je	.LBB31_16
	jmp	.LBB31_15
.LBB31_11:
	movb	$14, 239(%rsp)
	movzbl	239(%rsp), %esi
.Ltmp51:
	leaq	.L__unnamed_5(%rip), %rdx
	leaq	216(%rsp), %rdi
	movl	$33, %ecx
	callq	_ZN3std2io5error5Error3new17h6f48cdf0f4bdd9ebE
.Ltmp52:
	jmp	.LBB31_22
.LBB31_12:
	movq	128(%rsp), %rax
	addq	88(%rsp), %rax
	movq	%rax, 88(%rsp)
	jmp	.LBB31_21
.LBB31_13:
	movb	$0, 327(%rsp)
	movups	128(%rsp), %xmm0
	movaps	%xmm0, 160(%rsp)
	movaps	160(%rsp), %xmm0
	movaps	%xmm0, 256(%rsp)
	movaps	256(%rsp), %xmm0
	movaps	%xmm0, 240(%rsp)
.Ltmp46:
	leaq	96(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
.Ltmp47:
	jmp	.LBB31_39
.LBB31_14:
	cmpq	$0, 128(%rsp)
	je	.LBB31_11
	jmp	.LBB31_12
.LBB31_15:
	ud2
.LBB31_16:
	leaq	128(%rsp), %rax
	movq	%rax, 152(%rsp)
	movq	152(%rsp), %rdi
.Ltmp42:
	movq	_ZN3std2io5error5Error4kind17hbc9b63d37b78e2cfE@GOTPCREL(%rip), %rax
	callq	*%rax
.Ltmp43:
	movb	%al, 7(%rsp)
	jmp	.LBB31_17
.LBB31_17:
	movb	7(%rsp), %al
	movb	%al, 191(%rsp)
.Ltmp44:
	leaq	.L__unnamed_6(%rip), %rsi
	leaq	191(%rsp), %rdi
	callq	_ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE
.Ltmp45:
	movb	%al, 6(%rsp)
	jmp	.LBB31_19
.LBB31_18:
	cmpq	$1, 120(%rsp)
	je	.LBB31_29
	jmp	.LBB31_31
.LBB31_19:
	movb	6(%rsp), %al
	testb	$1, %al
	jne	.LBB31_20
	jmp	.LBB31_13
.LBB31_20:
.LBB31_21:
	movb	$0, 327(%rsp)
.Ltmp49:
	leaq	120(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
.Ltmp50:
	jmp	.LBB31_24
.LBB31_22:
	movups	216(%rsp), %xmm0
	movaps	%xmm0, 192(%rsp)
.Ltmp54:
	leaq	96(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
.Ltmp55:
	jmp	.LBB31_33
.LBB31_23:
	cmpq	$1, 120(%rsp)
	je	.LBB31_35
	jmp	.LBB31_37
.LBB31_24:
	movb	$0, 327(%rsp)
	jmp	.LBB31_3
.LBB31_25:
	movq	88(%rsp), %rax
	movq	%rax, 312(%rsp)
	movq	312(%rsp), %rdx
.Ltmp57:
	leaq	272(%rsp), %rdi
	movq	64(%rsp), %rsi
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$5drain17h949fd3b03a3b4fd8E
.Ltmp58:
	jmp	.LBB31_26
.LBB31_26:
.Ltmp59:
	leaq	272(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h8d0f01beada50793E
.Ltmp60:
	jmp	.LBB31_27
.LBB31_27:
	jmp	.LBB31_28
.LBB31_28:
	movq	96(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	104(%rsp), %rax
	movq	%rax, 8(%rcx)
	movq	72(%rsp), %rax
	addq	$344, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB31_29:
	.cfi_def_cfa_offset 352
	testb	$1, 327(%rsp)
	je	.LBB31_7
	movb	$0, 327(%rsp)
	leaq	120(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E
	jmp	.LBB31_7
.LBB31_31:
	leaq	120(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	jmp	.LBB31_7
.LBB31_32:
	movq	192(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 104(%rsp)
	jmp	.LBB31_18
.LBB31_33:
	movq	192(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 104(%rsp)
	jmp	.LBB31_23
.LBB31_34:
	movb	$0, 327(%rsp)
	jmp	.LBB31_4
.LBB31_35:
	testb	$1, 327(%rsp)
	je	.LBB31_34
	movb	$0, 327(%rsp)
	leaq	120(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E
	jmp	.LBB31_34
.LBB31_37:
	leaq	120(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	jmp	.LBB31_34
.LBB31_38:
	movq	240(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 104(%rsp)
	jmp	.LBB31_18
.LBB31_39:
	movq	240(%rsp), %rax
	movq	%rax, 96(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 104(%rsp)
	jmp	.LBB31_23
.LBB31_40:
.Ltmp61:
	movl	%edx, %ecx
	movq	%rax, 328(%rsp)
	movl	%ecx, 336(%rsp)
	jmp	.LBB31_7
.LBB31_41:
.Ltmp53:
	movl	%edx, %ecx
	movq	%rax, 328(%rsp)
	movl	%ecx, 336(%rsp)
	jmp	.LBB31_18
.LBB31_42:
.Ltmp48:
	movl	%edx, %ecx
	movq	%rax, 328(%rsp)
	movl	%ecx, 336(%rsp)
	jmp	.LBB31_38
.LBB31_43:
.Ltmp56:
	movl	%edx, %ecx
	movq	%rax, 328(%rsp)
	movl	%ecx, 336(%rsp)
	jmp	.LBB31_32
.Lfunc_end31:
	.size	_ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E, .Lfunc_end31-_ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table31:
.Lexception4:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end4-.Lcst_begin4
.Lcst_begin4:
	.uleb128 .Lfunc_begin4-.Lfunc_begin4
	.uleb128 .Ltmp34-.Lfunc_begin4
	.byte	0
	.byte	0
	.uleb128 .Ltmp34-.Lfunc_begin4
	.uleb128 .Ltmp37-.Ltmp34
	.uleb128 .Ltmp61-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp37-.Lfunc_begin4
	.uleb128 .Ltmp38-.Ltmp37
	.byte	0
	.byte	0
	.uleb128 .Ltmp38-.Lfunc_begin4
	.uleb128 .Ltmp41-.Ltmp38
	.uleb128 .Ltmp61-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp51-.Lfunc_begin4
	.uleb128 .Ltmp52-.Ltmp51
	.uleb128 .Ltmp53-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp46-.Lfunc_begin4
	.uleb128 .Ltmp47-.Ltmp46
	.uleb128 .Ltmp48-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp42-.Lfunc_begin4
	.uleb128 .Ltmp45-.Ltmp42
	.uleb128 .Ltmp53-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp49-.Lfunc_begin4
	.uleb128 .Ltmp50-.Ltmp49
	.uleb128 .Ltmp61-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp54-.Lfunc_begin4
	.uleb128 .Ltmp55-.Ltmp54
	.uleb128 .Ltmp56-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp57-.Lfunc_begin4
	.uleb128 .Ltmp60-.Ltmp57
	.uleb128 .Ltmp61-.Lfunc_begin4
	.byte	0
	.uleb128 .Ltmp60-.Lfunc_begin4
	.uleb128 .Lfunc_end31-.Ltmp60
	.byte	0
	.byte	0
.Lcst_end4:
	.p2align	2

	.section	".text._ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E,@function
_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movb	$0, 39(%rsp)
	movb	$1, 39(%rsp)
	movb	(%rdi), %al
	subb	$3, %al
	setne	%cl
	movzbl	%cl, %edx
	movl	%edx, %esi
	movq	%rdi, 24(%rsp)
	movb	%al, 23(%rsp)
	movq	%rsi, 8(%rsp)
	je	.LBB32_3
	jmp	.LBB32_9
.LBB32_9:
	movq	8(%rsp), %rax
	subq	$1, %rax
	movq	%rax, (%rsp)
	jne	.LBB32_2
	jmp	.LBB32_1
.LBB32_1:
	movb	$0, 38(%rsp)
	jmp	.LBB32_4
.LBB32_2:
	ud2
.LBB32_3:
	movb	$0, 39(%rsp)
	movb	$1, 38(%rsp)
.LBB32_4:
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	movq	24(%rsp), %rsi
	cmpb	$3, (%rsi)
	cmoveq	%rdx, %rcx
	cmpq	$0, %rcx
	je	.LBB32_6
	jmp	.LBB32_8
.LBB32_5:
	movb	38(%rsp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB32_6:
	.cfi_def_cfa_offset 48
	testb	$1, 39(%rsp)
	je	.LBB32_5
	movb	$0, 39(%rsp)
	jmp	.LBB32_5
.LBB32_8:
	movq	24(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
	jmp	.LBB32_5
.Lfunc_end32:
	.size	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E, .Lfunc_end32-_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E
	.cfi_endproc

	.section	".text._ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E,@function
_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E:
.Lfunc_begin5:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception5
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movb	$1, %al
	testb	%al, %al
	jne	.LBB33_3
	jmp	.LBB33_11
.LBB33_11:
	xorl	%eax, %eax
	movb	%al, %cl
	testb	%cl, %cl
	jne	.LBB33_4
	jmp	.LBB33_2
.LBB33_1:
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB33_2:
	ud2
.LBB33_3:
	movb	$1, %al
	testb	$1, %al
	jne	.LBB33_7
	jmp	.LBB33_8
.LBB33_4:
.Ltmp62:
	leaq	.L__unnamed_7(%rip), %rdi
	movl	$43, %esi
	callq	_ZN4core6result13unwrap_failed17he44ed40b4f33b687E
.Ltmp63:
	jmp	.LBB33_9
.LBB33_5:
	movb	$1, %al
	testb	$1, %al
	jne	.LBB33_6
	jmp	.LBB33_1
.LBB33_6:
	jmp	.LBB33_1
.LBB33_7:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB33_8:
	.cfi_def_cfa_offset 32
	jmp	.LBB33_7
.LBB33_9:
	ud2
.LBB33_10:
.Ltmp64:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB33_5
.Lfunc_end33:
	.size	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E, .Lfunc_end33-_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table33:
.Lexception5:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end5-.Lcst_begin5
.Lcst_begin5:
	.uleb128 .Lfunc_begin5-.Lfunc_begin5
	.uleb128 .Ltmp62-.Lfunc_begin5
	.byte	0
	.byte	0
	.uleb128 .Ltmp62-.Lfunc_begin5
	.uleb128 .Ltmp63-.Ltmp62
	.uleb128 .Ltmp64-.Lfunc_begin5
	.byte	0
.Lcst_end5:
	.p2align	2

	.section	".text._ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E,@function
_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	cmpq	$0, 8(%rdi)
	movq	%rdi, 32(%rsp)
	jne	.LBB34_2
	movq	$0, 48(%rsp)
	jmp	.LBB34_6
.LBB34_2:
	callq	_ZN4core3mem8align_of17h71d8720b645b9442E
	movq	%rax, 24(%rsp)
	callq	_ZN4core3mem7size_of17h74ed6d9378771763E
	movq	%rax, 16(%rsp)
	movq	16(%rsp), %rax
	movq	32(%rsp), %rcx
	imulq	8(%rcx), %rax
	movq	%rax, %rdi
	movq	24(%rsp), %rsi
	callq	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	8(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	(%rsp), %rcx
	movq	%rcx, 48(%rsp)
.LBB34_6:
	movq	40(%rsp), %rax
	movq	48(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end34:
	.size	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E, .Lfunc_end34-_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E
	.cfi_endproc

	.section	".text._ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E,@function
_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movq	%rdi, 48(%rsp)
	callq	_ZN4core3mem7size_of17h74ed6d9378771763E
	movq	%rax, 40(%rsp)
	movq	40(%rsp), %rax
	cmpq	$0, %rax
	je	.LBB35_9
	movq	48(%rsp), %rdi
	callq	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14current_layout17hd8729c99a9f32257E
	movq	%rdx, 64(%rsp)
	movq	%rax, 56(%rsp)
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	cmpq	$0, 64(%rsp)
	cmoveq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB35_5
	movq	56(%rsp), %rdx
	movq	64(%rsp), %rcx
	movq	48(%rsp), %rax
	movq	48(%rsp), %rsi
	movq	(%rsi), %rdi
	movq	%rdx, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	callq	_ZN101_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$core..ptr..Unique$LT$T$GT$$GT$$GT$4from17ha66817f0f4ab6f38E
	movq	%rax, 8(%rsp)
	jmp	.LBB35_6
.LBB35_5:
	jmp	.LBB35_9
.LBB35_6:
	movq	8(%rsp), %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$4cast17hab79b6d2e7b408c6E
	movq	%rax, (%rsp)
	movq	16(%rsp), %rdi
	movq	(%rsp), %rsi
	movq	32(%rsp), %rdx
	movq	24(%rsp), %rcx
	callq	_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E
	jmp	.LBB35_5
.LBB35_9:
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end35:
	.size	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E, .Lfunc_end35-_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E
	.cfi_endproc

	.section	".text._ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE,@function
_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h7f6d3e320c98b6e0E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end36:
	.size	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE, .Lfunc_end36-_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE
	.cfi_endproc

	.section	.text._ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E,@function
_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rsi, 40(%rsp)
	movq	40(%rsp), %rsi
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	16(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	48(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end37:
	.size	_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E, .Lfunc_end37-_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E
	.cfi_endproc

	.section	.text._ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E,@function
_ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rsi, 40(%rsp)
	movq	40(%rsp), %rsi
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	16(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	48(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end38:
	.size	_ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E, .Lfunc_end38-_ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E
	.cfi_endproc

	.section	.text._ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E,@function
_ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rsi, 40(%rsp)
	movq	40(%rsp), %rsi
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	16(%rsp), %rax
	movq	%rax, 48(%rsp)
	movq	48(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end39:
	.size	_ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E, .Lfunc_end39-_ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E
	.cfi_endproc

	.section	.text._ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE,@function
_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, %rax
	movq	24(%rsp), %r10
	movq	%r9, (%rsp)
	movq	%r10, 8(%rsp)
	movq	%rsi, (%rdi)
	movq	%rdx, 8(%rdi)
	movq	(%rsp), %rdx
	movq	8(%rsp), %rsi
	movq	%rdx, 16(%rdi)
	movq	%rsi, 24(%rdi)
	movq	%rcx, 32(%rdi)
	movq	%r8, 40(%rdi)
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end40:
	.size	_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE, .Lfunc_end40-_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE
	.cfi_endproc

	.section	.text._ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE,@function
_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, %rax
	movq	$0, (%rsp)
	movq	%rsi, (%rdi)
	movq	%rdx, 8(%rdi)
	movq	(%rsp), %rdx
	movq	8(%rsp), %rsi
	movq	%rdx, 16(%rdi)
	movq	%rsi, 24(%rdi)
	movq	%rcx, 32(%rdi)
	movq	%r8, 40(%rdi)
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end41:
	.size	_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE, .Lfunc_end41-_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE
	.cfi_endproc

	.section	.text._ZN4core3mem11size_of_val17hdcf217405823bb08E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem11size_of_val17hdcf217405823bb08E,@function
_ZN4core3mem11size_of_val17hdcf217405823bb08E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	$80, 16(%rsp)
	movq	16(%rsp), %rax
	movq	%rdi, 8(%rsp)
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end42:
	.size	_ZN4core3mem11size_of_val17hdcf217405823bb08E, .Lfunc_end42-_ZN4core3mem11size_of_val17hdcf217405823bb08E
	.cfi_endproc

	.section	.text._ZN4core3mem12align_of_val17hb60e844a77b184ddE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem12align_of_val17hb60e844a77b184ddE,@function
_ZN4core3mem12align_of_val17hb60e844a77b184ddE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	$8, 16(%rsp)
	movq	16(%rsp), %rax
	movq	%rdi, 8(%rsp)
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end43:
	.size	_ZN4core3mem12align_of_val17hb60e844a77b184ddE, .Lfunc_end43-_ZN4core3mem12align_of_val17hb60e844a77b184ddE
	.cfi_endproc

	.section	.text._ZN4core3mem13uninitialized17h4a20bd72620a0f45E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem13uninitialized17h4a20bd72620a0f45E,@function
_ZN4core3mem13uninitialized17h4a20bd72620a0f45E:
	.cfi_startproc
	subq	$2, %rsp
	.cfi_def_cfa_offset 10
	movb	1(%rsp), %al
	movb	%al, (%rsp)
	movb	(%rsp), %al
	addq	$2, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end44:
	.size	_ZN4core3mem13uninitialized17h4a20bd72620a0f45E, .Lfunc_end44-_ZN4core3mem13uninitialized17h4a20bd72620a0f45E
	.cfi_endproc

	.section	.text._ZN4core3mem4drop17h43ec02e90ad9ee3fE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem4drop17h43ec02e90ad9ee3fE,@function
_ZN4core3mem4drop17h43ec02e90ad9ee3fE:
	.cfi_startproc
	jmp	.LBB45_1
.LBB45_1:
	retq
.Lfunc_end45:
	.size	_ZN4core3mem4drop17h43ec02e90ad9ee3fE, .Lfunc_end45-_ZN4core3mem4drop17h43ec02e90ad9ee3fE
	.cfi_endproc

	.section	.text._ZN4core3mem7size_of17h74ed6d9378771763E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem7size_of17h74ed6d9378771763E,@function
_ZN4core3mem7size_of17h74ed6d9378771763E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	$1, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end46:
	.size	_ZN4core3mem7size_of17h74ed6d9378771763E, .Lfunc_end46-_ZN4core3mem7size_of17h74ed6d9378771763E
	.cfi_endproc

	.section	.text._ZN4core3mem8align_of17h71d8720b645b9442E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3mem8align_of17h71d8720b645b9442E,@function
_ZN4core3mem8align_of17h71d8720b645b9442E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	$1, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end47:
	.size	_ZN4core3mem8align_of17h71d8720b645b9442E, .Lfunc_end47-_ZN4core3mem8align_of17h71d8720b645b9442E
	.cfi_endproc

	.section	.text._ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE,@function
_ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rdi
	movq	%rdi, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end48:
	.size	_ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE, .Lfunc_end48-_ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE
	.cfi_endproc

	.section	.text._ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E,@function
_ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E:
	.cfi_startproc
	movq	%rdi, %rax
	retq
.Lfunc_end49:
	.size	_ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E, .Lfunc_end49-_ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E
	.cfi_endproc

	.section	.text._ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E,@function
_ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%sil, %al
	movb	%al, 7(%rsp)
	movzbl	7(%rsp), %edi
	callq	_ZN4core3mem4drop17h43ec02e90ad9ee3fE
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end50:
	.size	_ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E, .Lfunc_end50-_ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E
	.cfi_endproc

	.section	.text._ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE,@function
_ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE:
.Lfunc_begin6:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception6
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 8(%rsp)
.Ltmp65:
	leaq	8(%rsp), %rdi
	callq	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E
.Ltmp66:
	movl	%eax, 4(%rsp)
	jmp	.LBB51_1
.LBB51_1:
	jmp	.LBB51_2
.LBB51_2:
	movl	4(%rsp), %eax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB51_3:
	.cfi_def_cfa_offset 48
	jmp	.LBB51_4
.LBB51_4:
	movq	24(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB51_5:
.Ltmp67:
	movl	%edx, %ecx
	movq	%rax, 24(%rsp)
	movl	%ecx, 32(%rsp)
	jmp	.LBB51_3
.Lfunc_end51:
	.size	_ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE, .Lfunc_end51-_ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table51:
.Lexception6:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end6-.Lcst_begin6
.Lcst_begin6:
	.uleb128 .Ltmp65-.Lfunc_begin6
	.uleb128 .Ltmp66-.Ltmp65
	.uleb128 .Ltmp67-.Lfunc_begin6
	.byte	0
	.uleb128 .Ltmp66-.Lfunc_begin6
	.uleb128 .Lfunc_end51-.Ltmp66
	.byte	0
	.byte	0
.Lcst_end6:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E,@function
_ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr13drop_in_place17h7299a198abdc9c70E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end52:
	.size	_ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E, .Lfunc_end52-_ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h268d93918fae70aaE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h268d93918fae70aaE,@function
_ZN4core3ptr13drop_in_place17h268d93918fae70aaE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movb	(%rdi), %al
	subb	$1, %al
	movq	%rdi, 16(%rsp)
	movb	%al, 15(%rsp)
	ja	.LBB53_2
	jmp	.LBB53_1
.LBB53_1:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB53_2:
	.cfi_def_cfa_offset 32
	movq	16(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE
	jmp	.LBB53_1
.Lfunc_end53:
	.size	_ZN4core3ptr13drop_in_place17h268d93918fae70aaE, .Lfunc_end53-_ZN4core3ptr13drop_in_place17h268d93918fae70aaE
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE,@function
_ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE:
.Lfunc_begin7:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception7
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rax
.Ltmp68:
	movq	%rdi, (%rsp)
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17h130dd53ec7692fb2E
.Ltmp69:
	jmp	.LBB54_3
.LBB54_1:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB54_2:
	.cfi_def_cfa_offset 32
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB54_3:
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	callq	_ZN5alloc5alloc8box_free17h169758f12f51a3c5E
	jmp	.LBB54_1
.LBB54_4:
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	callq	_ZN5alloc5alloc8box_free17h169758f12f51a3c5E
	jmp	.LBB54_2
.LBB54_5:
.Ltmp70:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB54_4
.Lfunc_end54:
	.size	_ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE, .Lfunc_end54-_ZN4core3ptr13drop_in_place17h3e0cb6b732ab7a9eE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table54:
.Lexception7:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end7-.Lcst_begin7
.Lcst_begin7:
	.uleb128 .Ltmp68-.Lfunc_begin7
	.uleb128 .Ltmp69-.Ltmp68
	.uleb128 .Ltmp70-.Lfunc_begin7
	.byte	0
	.uleb128 .Ltmp69-.Lfunc_begin7
	.uleb128 .Lfunc_end54-.Ltmp69
	.byte	0
	.byte	0
.Lcst_end7:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17h412d70f47c1915f2E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E,@function
_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E:
.Lfunc_begin8:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception8
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
.Ltmp71:
	movq	%rdi, (%rsp)
	callq	_ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E
.Ltmp72:
	jmp	.LBB55_4
.LBB55_1:
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB55_2:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB55_3:
	.cfi_def_cfa_offset 32
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E
	jmp	.LBB55_1
.LBB55_4:
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E
	jmp	.LBB55_2
.LBB55_5:
.Ltmp73:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB55_3
.Lfunc_end55:
	.size	_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E, .Lfunc_end55-_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table55:
.Lexception8:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end8-.Lcst_begin8
.Lcst_begin8:
	.uleb128 .Ltmp71-.Lfunc_begin8
	.uleb128 .Ltmp72-.Ltmp71
	.uleb128 .Ltmp73-.Lfunc_begin8
	.byte	0
	.uleb128 .Ltmp72-.Lfunc_begin8
	.uleb128 .Lfunc_end55-.Ltmp72
	.byte	0
	.byte	0
.Lcst_end8:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17h459e1144589f88b7E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h459e1144589f88b7E,@function
_ZN4core3ptr13drop_in_place17h459e1144589f88b7E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, (%rsp)
	jmp	.LBB56_2
.LBB56_1:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB56_2:
	.cfi_def_cfa_offset 32
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	callq	_ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE
	jmp	.LBB56_1
.Lfunc_end56:
	.size	_ZN4core3ptr13drop_in_place17h459e1144589f88b7E, .Lfunc_end56-_ZN4core3ptr13drop_in_place17h459e1144589f88b7E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E,@function
_ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr13drop_in_place17hb17a801c012f8476E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end57:
	.size	_ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E, .Lfunc_end57-_ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h7299a198abdc9c70E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h7299a198abdc9c70E,@function
_ZN4core3ptr13drop_in_place17h7299a198abdc9c70E:
.Lfunc_begin9:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception9
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rdi), %rax
	movq	8(%rdi), %rcx
	movq	(%rcx), %rcx
.Ltmp74:
	movq	%rdi, (%rsp)
	movq	%rax, %rdi
	callq	*%rcx
.Ltmp75:
	jmp	.LBB58_3
.LBB58_1:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB58_2:
	.cfi_def_cfa_offset 32
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB58_3:
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
	callq	_ZN5alloc5alloc8box_free17h73bad6711bc24e64E
	jmp	.LBB58_1
.LBB58_4:
	movq	(%rsp), %rax
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
	callq	_ZN5alloc5alloc8box_free17h73bad6711bc24e64E
	jmp	.LBB58_2
.LBB58_5:
.Ltmp76:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB58_4
.Lfunc_end58:
	.size	_ZN4core3ptr13drop_in_place17h7299a198abdc9c70E, .Lfunc_end58-_ZN4core3ptr13drop_in_place17h7299a198abdc9c70E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table58:
.Lexception9:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end9-.Lcst_begin9
.Lcst_begin9:
	.uleb128 .Ltmp74-.Lfunc_begin9
	.uleb128 .Ltmp75-.Ltmp74
	.uleb128 .Ltmp76-.Lfunc_begin9
	.byte	0
	.uleb128 .Ltmp75-.Lfunc_begin9
	.uleb128 .Lfunc_end58-.Ltmp75
	.byte	0
	.byte	0
.Lcst_end9:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E,@function
_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end59:
	.size	_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E, .Lfunc_end59-_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E,@function
_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	cmpq	$0, (%rdi)
	movq	%rdi, (%rsp)
	jne	.LBB60_2
.LBB60_1:
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.LBB60_2:
	.cfi_def_cfa_offset 16
	movq	(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E
	jmp	.LBB60_1
.Lfunc_end60:
	.size	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E, .Lfunc_end60-_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E,@function
_ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr13drop_in_place17h6a639afeaa8acc99E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end61:
	.size	_ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E, .Lfunc_end61-_ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h8d0f01beada50793E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h8d0f01beada50793E,@function
_ZN4core3ptr13drop_in_place17h8d0f01beada50793E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end62:
	.size	_ZN4core3ptr13drop_in_place17h8d0f01beada50793E, .Lfunc_end62-_ZN4core3ptr13drop_in_place17h8d0f01beada50793E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17h932f5182fa08a04fE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17h932f5182fa08a04fE,@function
_ZN4core3ptr13drop_in_place17h932f5182fa08a04fE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	*(%rsi)
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end63:
	.size	_ZN4core3ptr13drop_in_place17h932f5182fa08a04fE, .Lfunc_end63-_ZN4core3ptr13drop_in_place17h932f5182fa08a04fE
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17ha263f64279284448E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17ha263f64279284448E,@function
_ZN4core3ptr13drop_in_place17ha263f64279284448E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end64:
	.size	_ZN4core3ptr13drop_in_place17ha263f64279284448E, .Lfunc_end64-_ZN4core3ptr13drop_in_place17ha263f64279284448E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17hb17a801c012f8476E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hb17a801c012f8476E,@function
_ZN4core3ptr13drop_in_place17hb17a801c012f8476E:
.Lfunc_begin10:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception10
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
.Ltmp77:
	movq	%rdi, (%rsp)
	callq	_ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E
.Ltmp78:
	jmp	.LBB65_4
.LBB65_1:
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB65_2:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB65_3:
	.cfi_def_cfa_offset 32
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E
	jmp	.LBB65_1
.LBB65_4:
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17h412d70f47c1915f2E
	jmp	.LBB65_2
.LBB65_5:
.Ltmp79:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB65_3
.Lfunc_end65:
	.size	_ZN4core3ptr13drop_in_place17hb17a801c012f8476E, .Lfunc_end65-_ZN4core3ptr13drop_in_place17hb17a801c012f8476E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table65:
.Lexception10:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end10-.Lcst_begin10
.Lcst_begin10:
	.uleb128 .Ltmp77-.Lfunc_begin10
	.uleb128 .Ltmp78-.Ltmp77
	.uleb128 .Ltmp79-.Lfunc_begin10
	.byte	0
	.uleb128 .Ltmp78-.Lfunc_begin10
	.uleb128 .Lfunc_end65-.Ltmp78
	.byte	0
	.byte	0
.Lcst_end10:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E,@function
_ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end66:
	.size	_ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E, .Lfunc_end66-_ZN4core3ptr13drop_in_place17hb8ae307183c7bb41E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17hb8c36f2597322afdE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE,@function
_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$1, %eax
	movl	%eax, %ecx
	xorl	%eax, %eax
	movl	%eax, %edx
	cmpb	$3, (%rdi)
	cmoveq	%rdx, %rcx
	cmpq	$0, %rcx
	movq	%rdi, (%rsp)
	jne	.LBB67_2
.LBB67_1:
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.LBB67_2:
	.cfi_def_cfa_offset 16
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E
	jmp	.LBB67_1
.Lfunc_end67:
	.size	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE, .Lfunc_end67-_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE,@function
_ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE:
.Lfunc_begin11:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception11
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
.Ltmp80:
	movq	%rdi, (%rsp)
	callq	_ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE
.Ltmp81:
	jmp	.LBB68_6
.LBB68_1:
	movq	8(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB68_2:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB68_3:
	.cfi_def_cfa_offset 32
	movq	(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E
	jmp	.LBB68_1
.LBB68_4:
	movq	(%rsp), %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17h459e1144589f88b7E
	jmp	.LBB68_3
.LBB68_5:
	movq	(%rsp), %rax
	addq	$8, %rax
	movq	%rax, %rdi
	callq	_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E
	jmp	.LBB68_2
.LBB68_6:
.Ltmp83:
	movq	(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h459e1144589f88b7E
.Ltmp84:
	jmp	.LBB68_5
.LBB68_7:
.Ltmp82:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB68_4
.LBB68_8:
.Ltmp85:
	movl	%edx, %ecx
	movq	%rax, 8(%rsp)
	movl	%ecx, 16(%rsp)
	jmp	.LBB68_3
.Lfunc_end68:
	.size	_ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE, .Lfunc_end68-_ZN4core3ptr13drop_in_place17hc65e02c0b50ea49cE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table68:
.Lexception11:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end11-.Lcst_begin11
.Lcst_begin11:
	.uleb128 .Ltmp80-.Lfunc_begin11
	.uleb128 .Ltmp81-.Ltmp80
	.uleb128 .Ltmp82-.Lfunc_begin11
	.byte	0
	.uleb128 .Ltmp81-.Lfunc_begin11
	.uleb128 .Ltmp83-.Ltmp81
	.byte	0
	.byte	0
	.uleb128 .Ltmp83-.Lfunc_begin11
	.uleb128 .Ltmp84-.Ltmp83
	.uleb128 .Ltmp85-.Lfunc_begin11
	.byte	0
.Lcst_end11:
	.p2align	2

	.section	.text._ZN4core3ptr13drop_in_place17hc689bce2a8b02479E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E,@function
_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end69:
	.size	_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E, .Lfunc_end69-_ZN4core3ptr13drop_in_place17hc689bce2a8b02479E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17hed52d9c15de89af2E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E,@function
_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	addq	$8, %rdi
	callq	_ZN4core3ptr13drop_in_place17h868c0a7d4144ca33E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end70:
	.size	_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E, .Lfunc_end70-_ZN4core3ptr13drop_in_place17hed52d9c15de89af2E
	.cfi_endproc

	.section	.text._ZN4core3ptr13drop_in_place17hfa2e74f292574248E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E,@function
_ZN4core3ptr13drop_in_place17hfa2e74f292574248E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr13drop_in_place17h268d93918fae70aaE
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end71:
	.size	_ZN4core3ptr13drop_in_place17hfa2e74f292574248E, .Lfunc_end71-_ZN4core3ptr13drop_in_place17hfa2e74f292574248E
	.cfi_endproc

	.section	".text._ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE,@function
_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end72:
	.size	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE, .Lfunc_end72-_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE
	.cfi_endproc

	.section	".text._ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE,@function
_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	addq	%rsi, %rdi
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end73:
	.size	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE, .Lfunc_end73-_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$6offset17hf871db48e3ef419eE
	.cfi_endproc

	.section	".text._ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE,@function
_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	callq	_ZN4core3ptr8null_mut17h61b58067ac4b652cE
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rcx
	cmpq	%rcx, %rax
	sete	%dl
	andb	$1, %dl
	movzbl	%dl, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end74:
	.size	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE, .Lfunc_end74-_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE
	.cfi_endproc

	.section	".text._ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E,@function
_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end75:
	.size	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E, .Lfunc_end75-_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E
	.cfi_endproc

	.section	".text._ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E,@function
_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	addq	%rsi, %rdi
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end76:
	.size	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E, .Lfunc_end76-_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E
	.cfi_endproc

	.section	".text._ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE,@function
_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end77:
	.size	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE, .Lfunc_end77-_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE
	.cfi_endproc

	.section	".text._ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE,@function
_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	addq	%rsi, %rdi
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end78:
	.size	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE, .Lfunc_end78-_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE
	.cfi_endproc

	.section	".text._ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E,@function
_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	callq	_ZN4core3ptr4null17h7ff53ecf6bd2e814E
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rcx
	cmpq	%rcx, %rax
	sete	%dl
	andb	$1, %dl
	movzbl	%dl, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end79:
	.size	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E, .Lfunc_end79-_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E
	.cfi_endproc

	.section	.text._ZN4core3ptr4null17h7ff53ecf6bd2e814E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr4null17h7ff53ecf6bd2e814E,@function
_ZN4core3ptr4null17h7ff53ecf6bd2e814E:
	.cfi_startproc
	xorl	%eax, %eax
	retq
.Lfunc_end80:
	.size	_ZN4core3ptr4null17h7ff53ecf6bd2e814E, .Lfunc_end80-_ZN4core3ptr4null17h7ff53ecf6bd2e814E
	.cfi_endproc

	.section	.text._ZN4core3ptr4read17h21339182e0e841b1E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr4read17h21339182e0e841b1E,@function
_ZN4core3ptr4read17h21339182e0e841b1E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 8(%rsp)
	callq	_ZN4core3mem13uninitialized17h4a20bd72620a0f45E
	movb	%al, 23(%rsp)
	movq	8(%rsp), %rax
	movb	(%rax), %cl
	movb	%cl, 23(%rsp)
	movb	23(%rsp), %al
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end81:
	.size	_ZN4core3ptr4read17h21339182e0e841b1E, .Lfunc_end81-_ZN4core3ptr4read17h21339182e0e841b1E
	.cfi_endproc

	.section	.text._ZN4core3ptr8null_mut17h61b58067ac4b652cE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core3ptr8null_mut17h61b58067ac4b652cE,@function
_ZN4core3ptr8null_mut17h61b58067ac4b652cE:
	.cfi_startproc
	xorl	%eax, %eax
	retq
.Lfunc_end82:
	.size	_ZN4core3ptr8null_mut17h61b58067ac4b652cE, .Lfunc_end82-_ZN4core3ptr8null_mut17h61b58067ac4b652cE
	.cfi_endproc

	.section	.text._ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE,@function
_ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE:
.Lfunc_begin12:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception12
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 8(%rsp)
.Ltmp86:
	leaq	8(%rsp), %rdi
	callq	_ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E
.Ltmp87:
	jmp	.LBB83_2
.LBB83_1:
	movq	24(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB83_2:
.Ltmp88:
	callq	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$6unwrap17h1da609701036e5c1E
.Ltmp89:
	jmp	.LBB83_4
.LBB83_3:
	jmp	.LBB83_1
.LBB83_4:
	jmp	.LBB83_5
.LBB83_5:
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB83_6:
	.cfi_def_cfa_offset 48
.Ltmp90:
	movl	%edx, %ecx
	movq	%rax, 24(%rsp)
	movl	%ecx, 32(%rsp)
	jmp	.LBB83_3
.Lfunc_end83:
	.size	_ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE, .Lfunc_end83-_ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table83:
.Lexception12:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end12-.Lcst_begin12
.Lcst_begin12:
	.uleb128 .Ltmp86-.Lfunc_begin12
	.uleb128 .Ltmp87-.Ltmp86
	.uleb128 .Ltmp90-.Lfunc_begin12
	.byte	0
	.uleb128 .Ltmp87-.Lfunc_begin12
	.uleb128 .Ltmp88-.Ltmp87
	.byte	0
	.byte	0
	.uleb128 .Ltmp88-.Lfunc_begin12
	.uleb128 .Ltmp89-.Ltmp88
	.uleb128 .Ltmp90-.Lfunc_begin12
	.byte	0
.Lcst_end12:
	.p2align	2

	.section	".text._ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE,@function
_ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%sil, %al
	movb	%al, 7(%rsp)
	movzbl	7(%rsp), %esi
	callq	_ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end84:
	.size	_ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE, .Lfunc_end84-_ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE
	.cfi_endproc

	.section	.text._ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E,@function
_ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	_ZN4core4iter8iterator8Iterator4fold17hbc0d9f5d6407846bE
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end85:
	.size	_ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E, .Lfunc_end85-_ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E
	.cfi_endproc

	.section	".text._ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE,@function
_ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%sil, %al
	movb	%al, 7(%rsp)
	movzbl	7(%rsp), %esi
	callq	_ZN4core3ops8function5FnMut8call_mut17hf5ad6cd3f7a248f0E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end86:
	.size	_ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE, .Lfunc_end86-_ZN4core4iter8iterator8Iterator8for_each28_$u7b$$u7b$closure$u7d$$u7d$17h27e7d85c903ccefcE
	.cfi_endproc

	.section	.text._ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E,@function
_ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E:
.Lfunc_begin13:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception13
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movb	$0, 55(%rsp)
	movb	$1, 55(%rsp)
	movq	%rdi, 8(%rsp)
	jmp	.LBB87_2
.LBB87_1:
	movq	56(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB87_2:
.Ltmp91:
	movq	8(%rsp), %rdi
	callq	_ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E
.Ltmp92:
	movb	%dl, 7(%rsp)
	movb	%al, 6(%rsp)
	jmp	.LBB87_5
.LBB87_3:
	jmp	.LBB87_1
.LBB87_4:
	testb	$1, 55(%rsp)
	jne	.LBB87_20
	jmp	.LBB87_3
.LBB87_5:
	movb	6(%rsp), %al
	movb	%al, 40(%rsp)
	movb	7(%rsp), %cl
	movb	%cl, 41(%rsp)
	movb	40(%rsp), %dl
	andb	$1, %dl
	movzbl	%dl, %esi
	movl	%esi, %edi
	cmpq	$1, %rdi
	je	.LBB87_7
	jmp	.LBB87_18
.LBB87_7:
	movb	41(%rsp), %al
	movb	$0, 55(%rsp)
	movb	%al, 54(%rsp)
	movzbl	54(%rsp), %esi
.Ltmp96:
	leaq	16(%rsp), %rdi
	callq	_ZN4core4iter8iterator8Iterator4fold28_$u7b$$u7b$closure$u7d$$u7d$17hd52b5d796b151b0dE
.Ltmp97:
	jmp	.LBB87_8
.LBB87_8:
.Ltmp98:
	callq	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E
.Ltmp99:
	jmp	.LBB87_10
.LBB87_9:
	movb	40(%rsp), %al
	andb	$1, %al
	movzbl	%al, %ecx
	movl	%ecx, %edx
	cmpq	$1, %rdx
	je	.LBB87_4
	jmp	.LBB87_21
.LBB87_10:
	movb	$1, %al
	testb	%al, %al
	jne	.LBB87_13
	jmp	.LBB87_34
.LBB87_34:
	xorl	%eax, %eax
	movb	%al, %cl
	testb	%cl, %cl
	jne	.LBB87_14
	jmp	.LBB87_12
.LBB87_11:
	movb	$1, %al
	testb	$1, %al
	jne	.LBB87_22
	jmp	.LBB87_9
.LBB87_12:
	ud2
.LBB87_13:
	movb	$1, 55(%rsp)
	movb	$1, %al
	testb	$1, %al
	jne	.LBB87_27
	jmp	.LBB87_28
.LBB87_14:
.Ltmp101:
	callq	_ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E
.Ltmp102:
	jmp	.LBB87_15
.LBB87_15:
.Ltmp103:
	callq	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE
.Ltmp104:
	jmp	.LBB87_16
.LBB87_16:
	movb	$1, %al
	testb	$1, %al
	jne	.LBB87_24
	jmp	.LBB87_23
.LBB87_17:
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB87_18:
	.cfi_def_cfa_offset 80
	movb	$0, 55(%rsp)
.Ltmp93:
	callq	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E
.Ltmp94:
	jmp	.LBB87_19
.LBB87_19:
	movb	$0, 55(%rsp)
	jmp	.LBB87_17
.LBB87_20:
	movb	$0, 55(%rsp)
	jmp	.LBB87_3
.LBB87_21:
	jmp	.LBB87_4
.LBB87_22:
	jmp	.LBB87_9
.LBB87_23:
	movb	40(%rsp), %al
	andb	$1, %al
	movzbl	%al, %ecx
	movl	%ecx, %edx
	cmpq	$1, %rdx
	je	.LBB87_25
	jmp	.LBB87_26
.LBB87_24:
	jmp	.LBB87_23
.LBB87_25:
	movb	$0, 55(%rsp)
	jmp	.LBB87_17
.LBB87_26:
	jmp	.LBB87_25
.LBB87_27:
	movb	40(%rsp), %al
	andb	$1, %al
	movzbl	%al, %ecx
	movl	%ecx, %edx
	cmpq	$1, %rdx
	je	.LBB87_29
	jmp	.LBB87_30
.LBB87_28:
	jmp	.LBB87_27
.LBB87_29:
	jmp	.LBB87_2
.LBB87_30:
	jmp	.LBB87_29
.LBB87_31:
.Ltmp95:
	movl	%edx, %ecx
	movq	%rax, 56(%rsp)
	movl	%ecx, 64(%rsp)
	jmp	.LBB87_4
.LBB87_32:
.Ltmp100:
	movl	%edx, %ecx
	movq	%rax, 56(%rsp)
	movl	%ecx, 64(%rsp)
	jmp	.LBB87_9
.LBB87_33:
.Ltmp105:
	movl	%edx, %ecx
	movq	%rax, 56(%rsp)
	movl	%ecx, 64(%rsp)
	jmp	.LBB87_11
.Lfunc_end87:
	.size	_ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E, .Lfunc_end87-_ZN4core4iter8iterator8Iterator8try_fold17h88d495169f4b3774E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table87:
.Lexception13:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end13-.Lcst_begin13
.Lcst_begin13:
	.uleb128 .Lfunc_begin13-.Lfunc_begin13
	.uleb128 .Ltmp91-.Lfunc_begin13
	.byte	0
	.byte	0
	.uleb128 .Ltmp91-.Lfunc_begin13
	.uleb128 .Ltmp92-.Ltmp91
	.uleb128 .Ltmp95-.Lfunc_begin13
	.byte	0
	.uleb128 .Ltmp96-.Lfunc_begin13
	.uleb128 .Ltmp99-.Ltmp96
	.uleb128 .Ltmp100-.Lfunc_begin13
	.byte	0
	.uleb128 .Ltmp101-.Lfunc_begin13
	.uleb128 .Ltmp104-.Ltmp101
	.uleb128 .Ltmp105-.Lfunc_begin13
	.byte	0
	.uleb128 .Ltmp93-.Lfunc_begin13
	.uleb128 .Ltmp94-.Ltmp93
	.uleb128 .Ltmp95-.Lfunc_begin13
	.byte	0
.Lcst_end13:
	.p2align	2

	.section	.text._ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E,@function
_ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E:
	.cfi_startproc
	subq	$48, %rsp
	.cfi_def_cfa_offset 56
	movb	%dl, %al
	movb	%al, 39(%rsp)
	movzbl	39(%rsp), %edx
	movl	%edx, %ecx
	movq	%rcx, %r8
	subq	$4, %r8
	movq	%rsi, 24(%rsp)
	movq	%rdi, 16(%rsp)
	movq	%rcx, 8(%rsp)
	movq	%r8, (%rsp)
	ja	.LBB88_6
	leaq	.LJTI88_0(%rip), %rax
	movq	8(%rsp), %rcx
	movslq	(%rax,%rcx,4), %rdx
	addq	%rax, %rdx
	jmpq	*%rdx
.LBB88_1:
	movq	24(%rsp), %rax
	negq	%rax
	movq	16(%rsp), %rcx
	lock		xaddq	%rax, (%rcx)
	movq	%rax, 40(%rsp)
	jmp	.LBB88_8
.LBB88_2:
	movq	24(%rsp), %rax
	negq	%rax
	movq	16(%rsp), %rcx
	lock		xaddq	%rax, (%rcx)
	movq	%rax, 40(%rsp)
	jmp	.LBB88_9
.LBB88_3:
	movq	24(%rsp), %rax
	negq	%rax
	movq	16(%rsp), %rcx
	lock		xaddq	%rax, (%rcx)
	movq	%rax, 40(%rsp)
	jmp	.LBB88_10
.LBB88_4:
	movq	24(%rsp), %rax
	negq	%rax
	movq	16(%rsp), %rcx
	lock		xaddq	%rax, (%rcx)
	movq	%rax, 40(%rsp)
	jmp	.LBB88_11
.LBB88_5:
	movq	24(%rsp), %rax
	negq	%rax
	movq	16(%rsp), %rcx
	lock		xaddq	%rax, (%rcx)
	movq	%rax, 40(%rsp)
	jmp	.LBB88_12
.LBB88_6:
	ud2
.LBB88_7:
	movq	40(%rsp), %rax
	addq	$48, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB88_8:
	.cfi_def_cfa_offset 56
	jmp	.LBB88_7
.LBB88_9:
	jmp	.LBB88_7
.LBB88_10:
	jmp	.LBB88_7
.LBB88_11:
	jmp	.LBB88_7
.LBB88_12:
	jmp	.LBB88_7
.Lfunc_end88:
	.size	_ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E, .Lfunc_end88-_ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E
	.cfi_endproc
	.section	.rodata._ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E,"a",@progbits
	.p2align	2
.LJTI88_0:
	.long	.LBB88_4-.LJTI88_0
	.long	.LBB88_2-.LJTI88_0
	.long	.LBB88_1-.LJTI88_0
	.long	.LBB88_3-.LJTI88_0
	.long	.LBB88_5-.LJTI88_0

	.section	.text._ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E,@function
_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movb	%dl, %al
	movq	%rsi, 32(%rsp)
	movb	%al, 31(%rsp)
	callq	_ZN40_$LT$core..cell..UnsafeCell$LT$T$GT$$GT$3get17ha6ff9d355aaa63d5E
	movq	%rax, 16(%rsp)
	movq	16(%rsp), %rdi
	movq	32(%rsp), %rsi
	movb	31(%rsp), %al
	movzbl	%al, %edx
	callq	_ZN4core4sync6atomic10atomic_sub17h8578432c848f4e49E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end89:
	.size	_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E, .Lfunc_end89-_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E
	.cfi_endproc

	.section	.text._ZN4core4sync6atomic5fence17h20ad0752fee06055E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core4sync6atomic5fence17h20ad0752fee06055E,@function
_ZN4core4sync6atomic5fence17h20ad0752fee06055E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movb	%dil, %al
	movb	%al, 31(%rsp)
	movzbl	31(%rsp), %edi
	movl	%edi, %ecx
	movq	%rcx, %rdx
	subq	$4, %rdx
	movq	%rcx, 16(%rsp)
	movq	%rdx, 8(%rsp)
	ja	.LBB90_6
	leaq	.LJTI90_0(%rip), %rax
	movq	16(%rsp), %rcx
	movslq	(%rax,%rcx,4), %rdx
	addq	%rax, %rdx
	jmpq	*%rdx
.LBB90_1:
	#MEMBARRIER
	jmp	.LBB90_7
.LBB90_2:
	#MEMBARRIER
	jmp	.LBB90_7
.LBB90_3:
	#MEMBARRIER
	jmp	.LBB90_7
.LBB90_4:
	mfence
	jmp	.LBB90_7
.LBB90_5:
	leaq	.L__unnamed_8(%rip), %rdi
	movq	_ZN4core9panicking5panic17hb30cf43f16b71b76E@GOTPCREL(%rip), %rax
	callq	*%rax
	ud2
.LBB90_6:
	ud2
.LBB90_7:
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end90:
	.size	_ZN4core4sync6atomic5fence17h20ad0752fee06055E, .Lfunc_end90-_ZN4core4sync6atomic5fence17h20ad0752fee06055E
	.cfi_endproc
	.section	.rodata._ZN4core4sync6atomic5fence17h20ad0752fee06055E,"a",@progbits
	.p2align	2
.LJTI90_0:
	.long	.LBB90_5-.LJTI90_0
	.long	.LBB90_2-.LJTI90_0
	.long	.LBB90_1-.LJTI90_0
	.long	.LBB90_3-.LJTI90_0
	.long	.LBB90_4-.LJTI90_0

	.section	.text._ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E,@function
_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	movq	%rsi, %rdi
	callq	_ZN4core3num12NonZeroUsize13new_unchecked17ha1db90e329702c3fE
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	8(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rdx
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end91:
	.size	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E, .Lfunc_end91-_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	.cfi_endproc

	.section	.text._ZN4core5alloc6Layout4size17h358112c93a843eceE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5alloc6Layout4size17h358112c93a843eceE,@function
_ZN4core5alloc6Layout4size17h358112c93a843eceE:
	.cfi_startproc
	movq	(%rdi), %rax
	retq
.Lfunc_end92:
	.size	_ZN4core5alloc6Layout4size17h358112c93a843eceE, .Lfunc_end92-_ZN4core5alloc6Layout4size17h358112c93a843eceE
	.cfi_endproc

	.section	.text._ZN4core5alloc6Layout5align17h47a0134f059f8f65E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5alloc6Layout5align17h47a0134f059f8f65E,@function
_ZN4core5alloc6Layout5align17h47a0134f059f8f65E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	8(%rdi), %rdi
	callq	_ZN4core3num12NonZeroUsize3get17h71d453a539c9d333E
	movq	%rax, (%rsp)
	movq	(%rsp), %rax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end93:
	.size	_ZN4core5alloc6Layout5align17h47a0134f059f8f65E, .Lfunc_end93-_ZN4core5alloc6Layout5align17h47a0134f059f8f65E
	.cfi_endproc

	.section	.text._ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE,@function
_ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 32(%rsp)
	callq	_ZN4core3mem11size_of_val17hdcf217405823bb08E
	movq	%rax, 24(%rsp)
	movq	32(%rsp), %rdi
	callq	_ZN4core3mem12align_of_val17hb60e844a77b184ddE
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	16(%rsp), %rcx
	movq	%rcx, 48(%rsp)
	movq	40(%rsp), %rdi
	movq	48(%rsp), %rsi
	callq	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	8(%rsp), %rax
	movq	(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end94:
	.size	_ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE, .Lfunc_end94-_ZN4core5alloc6Layout9for_value17h7c3fdad9c2e6566bE
	.cfi_endproc

	.section	.text._ZN4core5slice14from_raw_parts17h22690902781b2c88E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice14from_raw_parts17h22690902781b2c88E,@function
_ZN4core5slice14from_raw_parts17h22690902781b2c88E:
	.cfi_startproc
	subq	$32, %rsp
	.cfi_def_cfa_offset 40
	movq	%rdi, 16(%rsp)
	movq	%rsi, 24(%rsp)
	movq	16(%rsp), %rsi
	movq	24(%rsp), %rdi
	movq	%rsi, (%rsp)
	movq	%rdi, 8(%rsp)
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$32, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end95:
	.size	_ZN4core5slice14from_raw_parts17h22690902781b2c88E, .Lfunc_end95-_ZN4core5slice14from_raw_parts17h22690902781b2c88E
	.cfi_endproc

	.section	.text._ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE,@function
_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE:
	.cfi_startproc
	subq	$32, %rsp
	.cfi_def_cfa_offset 40
	movq	%rdi, 16(%rsp)
	movq	%rsi, 24(%rsp)
	movq	16(%rsp), %rsi
	movq	24(%rsp), %rdi
	movq	%rsi, (%rsp)
	movq	%rdi, 8(%rsp)
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$32, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end96:
	.size	_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE, .Lfunc_end96-_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE
	.cfi_endproc

	.section	".text._ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE,@function
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movq	%rsi, (%rsp)
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end97:
	.size	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE, .Lfunc_end97-_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE
	.cfi_endproc

	.section	".text._ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E,@function
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end98:
	.size	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E, .Lfunc_end98-_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	.cfi_endproc

	.section	".text._ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE,@function
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE:
	.cfi_startproc
	subq	$104, %rsp
	.cfi_def_cfa_offset 112
	movq	%rdi, 64(%rsp)
	movq	%rsi, 56(%rsp)
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE
	movq	%rax, 48(%rsp)
	movq	48(%rsp), %rdi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E
	movb	%al, 47(%rsp)
	callq	_ZN4core3mem7size_of17h74ed6d9378771763E
	movq	%rax, 32(%rsp)
	movq	32(%rsp), %rax
	cmpq	$0, %rax
	jne	.LBB99_6
	movq	64(%rsp), %rdi
	movq	56(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 24(%rsp)
	jmp	.LBB99_7
.LBB99_6:
	movq	64(%rsp), %rdi
	movq	56(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 16(%rsp)
	jmp	.LBB99_9
.LBB99_7:
	movq	48(%rsp), %rdi
	movq	24(%rsp), %rsi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$12wrapping_add17he0fa88a391d81d94E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, 88(%rsp)
	jmp	.LBB99_11
.LBB99_9:
	movq	48(%rsp), %rdi
	movq	16(%rsp), %rsi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE
	movq	%rax, 88(%rsp)
	jmp	.LBB99_11
.LBB99_11:
	movq	88(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 72(%rsp)
	movq	%rax, 80(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rdx
	addq	$104, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end99:
	.size	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE, .Lfunc_end99-_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$4iter17hd783b43e8ff6af4aE
	.cfi_endproc

	.section	".text._ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE,@function
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movq	%rsi, (%rsp)
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end100:
	.size	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE, .Lfunc_end100-_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE
	.cfi_endproc

	.section	".text._ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE,@function
_ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 32(%rsp)
	movq	%rdx, %rdi
	movq	32(%rsp), %rdx
	movq	%rsi, 24(%rsp)
	movq	%rdx, %rsi
	movq	24(%rsp), %rdx
	callq	_ZN103_$LT$core..ops..range..RangeFrom$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h564fc07aba9eec01E
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end101:
	.size	_ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE, .Lfunc_end101-_ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE
	.cfi_endproc

	.section	".text._ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE,@function
_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	_ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end102:
	.size	_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE, .Lfunc_end102-_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE
	.cfi_endproc

	.section	.text._ZN4core6result13unwrap_failed17he44ed40b4f33b687E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4core6result13unwrap_failed17he44ed40b4f33b687E,@function
_ZN4core6result13unwrap_failed17he44ed40b4f33b687E:
.Lfunc_begin14:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception14
	subq	$200, %rsp
	.cfi_def_cfa_offset 208
	movq	%rdi, 64(%rsp)
	movq	%rsi, 72(%rsp)
	leaq	64(%rsp), %rsi
	movq	%rsi, 168(%rsp)
	leaq	80(%rsp), %rsi
	movq	%rsi, 176(%rsp)
	movq	168(%rsp), %rdi
	movq	176(%rsp), %rsi
.Ltmp106:
	leaq	_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h342fd43b91a2dfbeE(%rip), %rax
	movq	%rsi, 56(%rsp)
	movq	%rax, %rsi
	callq	_ZN4core3fmt10ArgumentV13new17hbe934287e8e1d986E
.Ltmp107:
	movq	%rdx, 48(%rsp)
	movq	%rax, 40(%rsp)
	jmp	.LBB103_1
.LBB103_1:
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
.Ltmp108:
	movq	_ZN42_$LT$$u21$$u20$as$u20$core..fmt..Debug$GT$3fmt17h3b0f9ad04f2c7a55E@GOTPCREL(%rip), %rsi
	movq	56(%rsp), %rdi
	movq	%rax, 32(%rsp)
	movq	%rcx, 24(%rsp)
	callq	_ZN4core3fmt10ArgumentV13new17h409ddc4c4872efc8E
.Ltmp109:
	movq	%rdx, 16(%rsp)
	movq	%rax, 8(%rsp)
	jmp	.LBB103_4
.LBB103_2:
	movq	184(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB103_3:
	jmp	.LBB103_2
.LBB103_4:
	movq	32(%rsp), %rax
	movq	%rax, 136(%rsp)
	movq	24(%rsp), %rcx
	movq	%rcx, 144(%rsp)
	movq	8(%rsp), %rdx
	movq	%rdx, 152(%rsp)
	movq	16(%rsp), %rsi
	movq	%rsi, 160(%rsp)
.Ltmp110:
	movq	%rsp, %rdi
	movq	$2, (%rdi)
	leaq	.L__unnamed_9(%rip), %rsi
	leaq	.L__unnamed_10(%rip), %r9
	leaq	88(%rsp), %rdi
	leaq	136(%rsp), %rcx
	movl	$2, %r8d
	movq	%r8, %rdx
	callq	_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE
.Ltmp111:
	jmp	.LBB103_5
.LBB103_5:
.Ltmp112:
	leaq	.L__unnamed_11(%rip), %rsi
	movq	_ZN4core9panicking9panic_fmt17h303d9400021e7fd8E@GOTPCREL(%rip), %rax
	leaq	88(%rsp), %rdi
	callq	*%rax
.Ltmp113:
	jmp	.LBB103_7
.LBB103_6:
.Ltmp114:
	movl	%edx, %ecx
	movq	%rax, 184(%rsp)
	movl	%ecx, 192(%rsp)
	jmp	.LBB103_3
.LBB103_7:
	ud2
.Lfunc_end103:
	.size	_ZN4core6result13unwrap_failed17he44ed40b4f33b687E, .Lfunc_end103-_ZN4core6result13unwrap_failed17he44ed40b4f33b687E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table103:
.Lexception14:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end14-.Lcst_begin14
.Lcst_begin14:
	.uleb128 .Ltmp106-.Lfunc_begin14
	.uleb128 .Ltmp109-.Ltmp106
	.uleb128 .Ltmp114-.Lfunc_begin14
	.byte	0
	.uleb128 .Ltmp109-.Lfunc_begin14
	.uleb128 .Ltmp110-.Ltmp109
	.byte	0
	.byte	0
	.uleb128 .Ltmp110-.Lfunc_begin14
	.uleb128 .Ltmp113-.Ltmp110
	.uleb128 .Ltmp114-.Lfunc_begin14
	.byte	0
.Lcst_end14:
	.p2align	2

	.section	".text._ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E,@function
_ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E:
	.cfi_startproc
	retq
.Lfunc_end104:
	.size	_ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E, .Lfunc_end104-_ZN50_$LT$T$u20$as$u20$core..convert..From$LT$T$GT$$GT$4from17h816a998eb2d5f013E
	.cfi_endproc

	.section	".text._ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E,@function
_ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	*_ZN3std5error221_$LT$impl$u20$core..convert..From$LT$$RF$$u27$b$u20$str$GT$$u20$for$u20$alloc..boxed..Box$LT$$LP$dyn$u20$std..error..Error$u20$$u2b$$u20$core..marker..Send$u20$$u2b$$u20$core..marker..Sync$u20$$u2b$$u20$$u27$a$RP$$GT$$GT$4from17h2479b88d6c935e7aE@GOTPCREL(%rip)
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end105:
	.size	_ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E, .Lfunc_end105-_ZN50_$LT$T$u20$as$u20$core..convert..Into$LT$U$GT$$GT$4into17h2194500a4b7527d2E
	.cfi_endproc

	.section	".text._ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE,@function
_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	callq	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E
	movl	%eax, 4(%rsp)
	movl	4(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end106:
	.size	_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE, .Lfunc_end106-_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17he9573633715a146eE
	.cfi_endproc

	.section	".text._ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E,@function
_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 32(%rsp)
	movq	%rsi, %rdi
	movq	%rdx, 24(%rsp)
	movq	%rcx, 16(%rsp)
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_ptr17hbbb5729df2493055E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rdi
	movq	24(%rsp), %rsi
	movq	16(%rsp), %rdx
	callq	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end107:
	.size	_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E, .Lfunc_end107-_ZN59_$LT$alloc..alloc..Global$u20$as$u20$core..alloc..Alloc$GT$7dealloc17hb18693bff6eabe69E
	.cfi_endproc

	.section	.text._ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE,@function
_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rsi, 24(%rsp)
	movq	%rdx, 32(%rsp)
	leaq	24(%rsp), %rdx
	movq	%rdi, 16(%rsp)
	movq	%rdx, %rdi
	callq	_ZN4core5alloc6Layout4size17h358112c93a843eceE
	movq	%rax, 8(%rsp)
	leaq	24(%rsp), %rdi
	callq	_ZN4core5alloc6Layout5align17h47a0134f059f8f65E
	movq	%rax, (%rsp)
	movq	16(%rsp), %rdi
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	callq	*__rust_dealloc@GOTPCREL(%rip)
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end108:
	.size	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE, .Lfunc_end108-_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE
	.cfi_endproc

	.section	.text._ZN5alloc5alloc8box_free17h169758f12f51a3c5E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN5alloc5alloc8box_free17h169758f12f51a3c5E,@function
_ZN5alloc5alloc8box_free17h169758f12f51a3c5E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	callq	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h3fefea6f4dba4fbfE
	movq	%rax, 32(%rsp)
	movq	$24, 40(%rsp)
	movq	40(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	$8, 48(%rsp)
	movq	48(%rsp), %rsi
	movq	%rsi, 16(%rsp)
	movq	24(%rsp), %rax
	cmpq	$0, %rax
	je	.LBB109_7
	movq	24(%rsp), %rdi
	movq	16(%rsp), %rsi
	callq	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	32(%rsp), %rax
	movq	%rax, %rdi
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	callq	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE
	jmp	.LBB109_7
.LBB109_7:
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end109:
	.size	_ZN5alloc5alloc8box_free17h169758f12f51a3c5E, .Lfunc_end109-_ZN5alloc5alloc8box_free17h169758f12f51a3c5E
	.cfi_endproc

	.section	.text._ZN5alloc5alloc8box_free17h73bad6711bc24e64E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN5alloc5alloc8box_free17h73bad6711bc24e64E,@function
_ZN5alloc5alloc8box_free17h73bad6711bc24e64E:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	callq	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h298d50c1cd52a885E
	movq	%rax, 48(%rsp)
	movq	%rdx, 40(%rsp)
	movq	40(%rsp), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)
	movq	56(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	40(%rsp), %rax
	movq	16(%rax), %rcx
	movq	%rcx, 64(%rsp)
	movq	64(%rsp), %rsi
	movq	%rsi, 24(%rsp)
	movq	32(%rsp), %rax
	cmpq	$0, %rax
	je	.LBB110_7
	movq	32(%rsp), %rdi
	movq	24(%rsp), %rsi
	callq	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	48(%rsp), %rax
	movq	%rax, %rdi
	movq	16(%rsp), %rsi
	movq	8(%rsp), %rdx
	callq	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE
	jmp	.LBB110_7
.LBB110_7:
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end110:
	.size	_ZN5alloc5alloc8box_free17h73bad6711bc24e64E, .Lfunc_end110-_ZN5alloc5alloc8box_free17h73bad6711bc24e64E
	.cfi_endproc

	.section	.text._ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE,@function
_ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	callq	_ZN35_$LT$core..ptr..Unique$LT$T$GT$$GT$6as_ptr17h46f3035f700b2ca9E
	movq	%rax, 32(%rsp)
	movq	$40, 40(%rsp)
	movq	40(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	$8, 48(%rsp)
	movq	48(%rsp), %rsi
	movq	%rsi, 16(%rsp)
	movq	24(%rsp), %rax
	cmpq	$0, %rax
	je	.LBB111_7
	movq	24(%rsp), %rdi
	movq	16(%rsp), %rsi
	callq	_ZN4core5alloc6Layout25from_size_align_unchecked17h03a01bb4553508e0E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	32(%rsp), %rax
	movq	%rax, %rdi
	movq	8(%rsp), %rsi
	movq	(%rsp), %rdx
	callq	_ZN5alloc5alloc7dealloc17hcdcb20fc5155189dE
	jmp	.LBB111_7
.LBB111_7:
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end111:
	.size	_ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE, .Lfunc_end111-_ZN5alloc5alloc8box_free17hb7ba8fc056181f9aE
	.cfi_endproc

	.section	".text._ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E,@function
_ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E
	movb	%al, 7(%rsp)
	movb	%dl, 6(%rsp)
	movb	7(%rsp), %al
	andb	$1, %al
	movb	6(%rsp), %dl
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end112:
	.size	_ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E, .Lfunc_end112-_ZN64_$LT$$RF$mut$u20$I$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb69bb85eaf35ea53E
	.cfi_endproc

	.section	".text._ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE,@function
_ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE:
.Lfunc_begin15:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception15
	subq	$120, %rsp
	.cfi_def_cfa_offset 128
	movq	%rdi, %rax
	movb	$0, 103(%rsp)
	movzbl	(%rsi), %r8d
	movl	%r8d, %r9d
	movb	%r9b, %r10b
	testb	$1, %r10b
	movq	%rax, 64(%rsp)
	movq	%rcx, 56(%rsp)
	movq	%rdx, 48(%rsp)
	movq	%rsi, 40(%rsp)
	movq	%rdi, 32(%rsp)
	movq	%r9, 24(%rsp)
	je	.LBB113_4
	jmp	.LBB113_13
.LBB113_13:
	movq	24(%rsp), %rax
	subq	$1, %rax
	movq	%rax, 16(%rsp)
	je	.LBB113_2
	jmp	.LBB113_3
.LBB113_1:
	movq	104(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB113_2:
	movq	48(%rsp), %rdi
	movq	56(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 8(%rsp)
	jmp	.LBB113_9
.LBB113_3:
	ud2
.LBB113_4:
	movq	40(%rsp), %rax
	addq	$1, %rax
	movb	$1, 103(%rsp)
	leaq	72(%rsp), %rdi
	movq	%rax, %rsi
	movq	48(%rsp), %rdx
	movq	56(%rsp), %rcx
	callq	*_ZN60_$LT$std..io..stdio..StdoutRaw$u20$as$u20$std..io..Write$GT$5write17hd43d2d17930e6744E@GOTPCREL(%rip)
	jmp	.LBB113_6
.LBB113_5:
	movq	64(%rsp), %rax
	addq	$120, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB113_6:
	.cfi_def_cfa_offset 128
.Ltmp115:
	movq	48(%rsp), %rdi
	movq	56(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
.Ltmp116:
	movq	%rax, (%rsp)
	jmp	.LBB113_7
.LBB113_7:
	movb	$0, 103(%rsp)
.Ltmp117:
	leaq	72(%rsp), %rsi
	movq	32(%rsp), %rdi
	movq	(%rsp), %rdx
	callq	_ZN3std2io5stdio12handle_ebadf17hb6861f914ce3570cE
.Ltmp118:
	jmp	.LBB113_8
.LBB113_8:
	movb	$0, 103(%rsp)
	jmp	.LBB113_5
.LBB113_9:
	movq	32(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	jmp	.LBB113_5
.LBB113_10:
	movb	$0, 103(%rsp)
	leaq	72(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7d48e79f3b5c2ea8E
	jmp	.LBB113_1
.LBB113_11:
	testb	$1, 103(%rsp)
	jne	.LBB113_10
	jmp	.LBB113_1
.LBB113_12:
.Ltmp119:
	movl	%edx, %ecx
	movq	%rax, 104(%rsp)
	movl	%ecx, 112(%rsp)
	jmp	.LBB113_11
.Lfunc_end113:
	.size	_ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE, .Lfunc_end113-_ZN65_$LT$std..io..stdio..Maybe$LT$W$GT$$u20$as$u20$std..io..Write$GT$5write17h31fb0954e829245cE
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table113:
.Lexception15:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end15-.Lcst_begin15
.Lcst_begin15:
	.uleb128 .Lfunc_begin15-.Lfunc_begin15
	.uleb128 .Ltmp115-.Lfunc_begin15
	.byte	0
	.byte	0
	.uleb128 .Ltmp115-.Lfunc_begin15
	.uleb128 .Ltmp118-.Ltmp115
	.uleb128 .Ltmp119-.Lfunc_begin15
	.byte	0
	.uleb128 .Ltmp118-.Lfunc_begin15
	.uleb128 .Lfunc_end113-.Ltmp118
	.byte	0
	.byte	0
.Lcst_end15:
	.p2align	2

	.section	".text._ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E,@function
_ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	callq	_ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	jmp	.LBB114_2
.LBB114_2:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end114:
	.size	_ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E, .Lfunc_end114-_ZN66_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h96946133fb902a61E
	.cfi_endproc

	.section	".text._ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE,@function
_ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movzbl	(%rdi), %eax
	movl	%eax, %ecx
	movq	%rcx, 56(%rsp)
	movq	56(%rsp), %rcx
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rcx, 8(%rsp)
	movq	16(%rsp), %rax
	movzbl	(%rax), %ecx
	movl	%ecx, %edx
	movq	%rdx, 64(%rsp)
	movq	64(%rsp), %rdx
	movq	%rdx, (%rsp)
	movq	8(%rsp), %rax
	movq	(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB115_4
	movb	$1, 39(%rsp)
	jmp	.LBB115_5
.LBB115_4:
	movb	$0, 39(%rsp)
.LBB115_5:
	testb	$1, 39(%rsp)
	je	.LBB115_7
	movq	24(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	16(%rsp), %rcx
	movq	%rcx, 48(%rsp)
	movb	$1, 38(%rsp)
	jmp	.LBB115_8
.LBB115_7:
	movb	$0, 38(%rsp)
.LBB115_8:
	movb	38(%rsp), %al
	andb	$1, %al
	movzbl	%al, %eax
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end115:
	.size	_ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE, .Lfunc_end115-_ZN66_$LT$std..io..error..ErrorKind$u20$as$u20$core..cmp..PartialEq$GT$2eq17h0ea0e4a22e6a037dE
	.cfi_endproc

	.section	".text._ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE,@function
_ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 24(%rsp)
	callq	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$5inner17hb12eab2f6022be79E
	movq	%rax, 16(%rsp)
	movl	$1, %eax
	movl	%eax, %esi
	movq	16(%rsp), %rcx
	movb	$1, 38(%rsp)
	movq	%rcx, %rdi
	movzbl	38(%rsp), %edx
	callq	_ZN4core4sync6atomic11AtomicUsize9fetch_sub17h44e6093d24fa0050E
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	cmpq	$1, %rax
	je	.LBB116_4
	jmp	.LBB116_5
.LBB116_4:
	movb	$2, 39(%rsp)
	movzbl	39(%rsp), %edi
	callq	_ZN4core4sync6atomic5fence17h20ad0752fee06055E
	jmp	.LBB116_6
.LBB116_5:
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB116_6:
	.cfi_def_cfa_offset 48
	movq	24(%rsp), %rdi
	callq	_ZN34_$LT$alloc..sync..Arc$LT$T$GT$$GT$9drop_slow17hed702ad433665753E
	jmp	.LBB116_5
.Lfunc_end116:
	.size	_ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE, .Lfunc_end116-_ZN67_$LT$alloc..sync..Arc$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h81176657edf55e1aE
	.cfi_endproc

	.section	".text._ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE,@function
_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, %rax
	movq	%rdi, 32(%rsp)
	movq	%rax, %rdi
	callq	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE
	movq	%rax, 24(%rsp)
	movq	24(%rsp), %rdi
	callq	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE
	movb	%al, 23(%rsp)
	movq	32(%rsp), %rax
	movq	16(%rax), %rsi
	movq	24(%rsp), %rdi
	callq	_ZN4core5slice14from_raw_parts17h22690902781b2c88E
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	8(%rsp), %rax
	movq	(%rsp), %rdx
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end117:
	.size	_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE, .Lfunc_end117-_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE
	.cfi_endproc

	.section	".text._ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E,@function
_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movb	%dil, %al
	movb	%al, 7(%rsp)
	leaq	7(%rsp), %rdi
	callq	_ZN3std3sys4unix7process14process_common8ExitCode6as_i3217h6e91f8692c20255aE
	movl	%eax, (%rsp)
	movl	(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end118:
	.size	_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E, .Lfunc_end118-_ZN68_$LT$std..process..ExitCode$u20$as$u20$std..process..Termination$GT$6report17hd62d97a5b23e4db8E
	.cfi_endproc

	.section	".text._ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE,@function
_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, %rax
	movq	%rdi, 32(%rsp)
	movq	%rax, %rdi
	callq	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$3ptr17h407c9bf30ab69f8fE
	movq	%rax, 24(%rsp)
	movq	24(%rsp), %rdi
	callq	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$7is_null17h88b7ee957cd9c99fE
	movb	%al, 23(%rsp)
	movq	32(%rsp), %rax
	movq	16(%rax), %rsi
	movq	24(%rsp), %rdi
	callq	_ZN4core5slice18from_raw_parts_mut17hebad16498b548a8dE
	movq	%rax, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	8(%rsp), %rax
	movq	(%rsp), %rdx
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end119:
	.size	_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE, .Lfunc_end119-_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE
	.cfi_endproc

	.section	".text._ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E,@function
_ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E:
.Lfunc_begin16:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception16
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movb	$0, 55(%rsp)
	movb	$1, 55(%rsp)
.Ltmp120:
	movq	%rsi, 40(%rsp)
	callq	_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE
.Ltmp121:
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
	jmp	.LBB120_2
.LBB120_1:
	movq	56(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB120_2:
	movb	$0, 55(%rsp)
.Ltmp122:
	movq	24(%rsp), %rdi
	movq	32(%rsp), %rsi
	movq	40(%rsp), %rdx
	callq	_ZN4core5slice74_$LT$impl$u20$core..ops..index..Index$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$5index17hdebd84530b2eef2cE
.Ltmp123:
	movq	%rdx, 16(%rsp)
	movq	%rax, 8(%rsp)
	jmp	.LBB120_3
.LBB120_3:
	movq	8(%rsp), %rax
	movq	16(%rsp), %rdx
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB120_4:
	.cfi_def_cfa_offset 80
	movb	$0, 55(%rsp)
	jmp	.LBB120_1
.LBB120_5:
	testb	$1, 55(%rsp)
	jne	.LBB120_4
	jmp	.LBB120_1
.LBB120_6:
.Ltmp124:
	movl	%edx, %ecx
	movq	%rax, 56(%rsp)
	movl	%ecx, 64(%rsp)
	jmp	.LBB120_5
.Lfunc_end120:
	.size	_ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E, .Lfunc_end120-_ZN77_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..Index$LT$I$GT$$GT$5index17hd6ff72fec5cbab52E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table120:
.Lexception16:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end16-.Lcst_begin16
.Lcst_begin16:
	.uleb128 .Ltmp120-.Lfunc_begin16
	.uleb128 .Ltmp121-.Ltmp120
	.uleb128 .Ltmp124-.Lfunc_begin16
	.byte	0
	.uleb128 .Ltmp121-.Lfunc_begin16
	.uleb128 .Ltmp122-.Ltmp121
	.byte	0
	.byte	0
	.uleb128 .Ltmp122-.Lfunc_begin16
	.uleb128 .Ltmp123-.Ltmp122
	.uleb128 .Ltmp124-.Lfunc_begin16
	.byte	0
.Lcst_end16:
	.p2align	2

	.section	".text._ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE,@function
_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end121:
	.size	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE, .Lfunc_end121-_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$10from_error17ha35e8057cd87edbeE
	.cfi_endproc

	.section	".text._ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E,@function
_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E:
	.cfi_startproc
	retq
.Lfunc_end122:
	.size	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E, .Lfunc_end122-_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$11into_result17hf8f66ca22b7ae1e3E
	.cfi_endproc

	.section	".text._ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E,@function
_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end123:
	.size	_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E, .Lfunc_end123-_ZN78_$LT$core..result..Result$LT$T$C$$u20$E$GT$$u20$as$u20$core..ops..try..Try$GT$7from_ok17h412212b40ebb29c7E
	.cfi_endproc

	.section	".text._ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E,@function
_ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, %rax
	addq	$24, %rax
	movq	%rdi, 8(%rsp)
	movq	%rax, %rdi
	callq	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$7is_some17h6c13f8ecfaeff104E
	movb	%al, 7(%rsp)
	jmp	.LBB124_5
.LBB124_1:
	movb	$1, 23(%rsp)
	jmp	.LBB124_4
.LBB124_2:
	movb	$0, 23(%rsp)
	jmp	.LBB124_4
.LBB124_3:
	movq	8(%rsp), %rax
	movb	25(%rax), %cl
	xorb	$-1, %cl
	testb	$1, %cl
	jne	.LBB124_1
	jmp	.LBB124_2
.LBB124_4:
	testb	$1, 23(%rsp)
	jne	.LBB124_6
	jmp	.LBB124_9
.LBB124_5:
	movb	7(%rsp), %al
	testb	$1, %al
	jne	.LBB124_3
	jmp	.LBB124_2
.LBB124_6:
	leaq	24(%rsp), %rdi
	movq	8(%rsp), %rsi
	callq	_ZN46_$LT$std..io..buffered..BufWriter$LT$W$GT$$GT$9flush_buf17h0e47ecf026ca7d00E
	leaq	24(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17hb8c36f2597322afdE
	jmp	.LBB124_9
.LBB124_9:
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end124:
	.size	_ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E, .Lfunc_end124-_ZN79_$LT$std..io..buffered..BufWriter$LT$W$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h9fb46348609987b6E
	.cfi_endproc

	.section	".text._ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E,@function
_ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E:
.Lfunc_begin17:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception17
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movb	$0, 39(%rsp)
	movb	$1, 39(%rsp)
.Ltmp125:
	callq	_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE
.Ltmp126:
	movq	%rdx, 24(%rsp)
	movq	%rax, 16(%rsp)
	jmp	.LBB125_2
.LBB125_1:
	movq	40(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB125_2:
	movb	$0, 39(%rsp)
.Ltmp127:
	movq	16(%rsp), %rdi
	movq	24(%rsp), %rsi
	callq	_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17h28af10b0b145afabE
.Ltmp128:
	movq	%rdx, 8(%rsp)
	movq	%rax, (%rsp)
	jmp	.LBB125_3
.LBB125_3:
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB125_4:
	.cfi_def_cfa_offset 64
	movb	$0, 39(%rsp)
	jmp	.LBB125_1
.LBB125_5:
	testb	$1, 39(%rsp)
	jne	.LBB125_4
	jmp	.LBB125_1
.LBB125_6:
.Ltmp129:
	movl	%edx, %ecx
	movq	%rax, 40(%rsp)
	movl	%ecx, 48(%rsp)
	jmp	.LBB125_5
.Lfunc_end125:
	.size	_ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E, .Lfunc_end125-_ZN80_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..index..IndexMut$LT$I$GT$$GT$9index_mut17hce0932369399d536E
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table125:
.Lexception17:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end17-.Lcst_begin17
.Lcst_begin17:
	.uleb128 .Ltmp125-.Lfunc_begin17
	.uleb128 .Ltmp126-.Ltmp125
	.uleb128 .Ltmp129-.Lfunc_begin17
	.byte	0
	.uleb128 .Ltmp126-.Lfunc_begin17
	.uleb128 .Ltmp127-.Ltmp126
	.byte	0
	.byte	0
	.uleb128 .Ltmp127-.Lfunc_begin17
	.uleb128 .Ltmp128-.Ltmp127
	.uleb128 .Ltmp129-.Lfunc_begin17
	.byte	0
.Lcst_end17:
	.p2align	2

	.section	".text._ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E,@function
_ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN49_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$GT$14dealloc_buffer17hb41697e755bbfa49E
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end126:
	.size	_ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E, .Lfunc_end126-_ZN82_$LT$alloc..raw_vec..RawVec$LT$T$C$$u20$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17hb2902dcc2e700f83E
	.cfi_endproc

	.section	".text._ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E,@function
_ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E:
	.cfi_startproc
	subq	$120, %rsp
	.cfi_def_cfa_offset 128
	movq	%rdi, 112(%rsp)
	callq	_ZN4core4iter8iterator8Iterator8for_each17h971facd428423239E
	movq	112(%rsp), %rax
	cmpq	$0, 8(%rax)
	jbe	.LBB127_15
	movq	112(%rsp), %rax
	addq	$32, %rax
	movq	%rax, %rdi
	callq	_ZN36_$LT$core..ptr..NonNull$LT$T$GT$$GT$6as_mut17hdc074a38d770a7f9E
	movq	%rax, 104(%rsp)
	movq	104(%rsp), %rdi
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$3len17hbcea5145c69bb85cE
	movq	%rax, 96(%rsp)
	movq	112(%rsp), %rax
	movq	(%rax), %rcx
	movq	96(%rsp), %rdx
	cmpq	%rdx, %rcx
	movq	%rcx, 88(%rsp)
	je	.LBB127_13
	movq	104(%rsp), %rdi
	callq	_ZN68_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..Deref$GT$5deref17h303b751f6f2d062aE
	movq	%rax, 80(%rsp)
	movq	%rdx, 72(%rsp)
	movq	80(%rsp), %rdi
	movq	72(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE
	movq	%rax, 64(%rsp)
	movq	64(%rsp), %rdi
	movq	88(%rsp), %rsi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE
	movq	%rax, 56(%rsp)
	movq	104(%rsp), %rdi
	callq	_ZN71_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$core..ops..deref..DerefMut$GT$9deref_mut17he8619ae520d55f3aE
	movq	%rax, 48(%rsp)
	movq	%rdx, 40(%rsp)
	movq	48(%rsp), %rdi
	movq	40(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$10as_mut_ptr17he28f2ebc6b49165cE
	movq	%rax, 32(%rsp)
	movq	32(%rsp), %rdi
	movq	96(%rsp), %rsi
	callq	_ZN4core3ptr31_$LT$impl$u20$$BP$mut$u20$T$GT$3add17h294838f90c13aecbE
	movq	%rax, 24(%rsp)
	movq	112(%rsp), %rax
	movq	8(%rax), %rcx
	shlq	$0, %rcx
	movq	memmove@GOTPCREL(%rip), %rdx
	movq	24(%rsp), %rdi
	movq	56(%rsp), %rsi
	movq	%rdx, 16(%rsp)
	movq	%rcx, %rdx
	movq	16(%rsp), %rcx
	callq	*%rcx
	movq	%rax, 8(%rsp)
	jmp	.LBB127_13
.LBB127_13:
	movq	96(%rsp), %rax
	movq	112(%rsp), %rcx
	addq	8(%rcx), %rax
	movq	104(%rsp), %rdi
	movq	%rax, %rsi
	callq	_ZN33_$LT$alloc..vec..Vec$LT$T$GT$$GT$7set_len17hecac5b8043652b81E
	jmp	.LBB127_15
.LBB127_15:
	addq	$120, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end127:
	.size	_ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E, .Lfunc_end127-_ZN82_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h4ad37410d44066f8E
	.cfi_endproc

	.section	".text._ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE,@function
_ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE:
	.cfi_startproc
	movq	%rdi, %rax
	movq	%rsi, %rdx
	retq
.Lfunc_end128:
	.size	_ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE, .Lfunc_end128-_ZN90_$LT$core..ops..range..RangeFull$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$9index_mut17hda34dfb51c03f95eE
	.cfi_endproc

	.section	".text._ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E,@function
_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	addq	$16, %rdi
	callq	_ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rdi
	callq	_ZN38_$LT$core..option..Option$LT$T$GT$$GT$3map17h9894fa8c4fb9e3b1E
	movb	%al, 7(%rsp)
	movb	%dl, 6(%rsp)
	movb	7(%rsp), %al
	andb	$1, %al
	movb	6(%rsp), %dl
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end129:
	.size	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E, .Lfunc_end129-_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17h9314ca45103be617E
	.cfi_endproc

	.section	".text._ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE,@function
_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_ZN4core3ptr4read17h21339182e0e841b1E
	movb	%al, 7(%rsp)
	movb	7(%rsp), %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end130:
	.size	_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE, .Lfunc_end130-_ZN91_$LT$alloc..vec..Drain$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next28_$u7b$$u7b$closure$u7d$$u7d$17hb26ff772b30e5b5dE
	.cfi_endproc

	.section	".text._ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE,@function
_ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movq	(%rdi), %rax
	movq	%rdi, 48(%rsp)
	movq	%rax, %rdi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E
	movb	%al, 47(%rsp)
	callq	_ZN4core3mem7size_of17h74ed6d9378771763E
	movq	%rax, 32(%rsp)
	movq	32(%rsp), %rax
	cmpq	$0, %rax
	je	.LBB131_7
	movq	48(%rsp), %rax
	movq	8(%rax), %rdi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$7is_null17h9ef1acd62a399d99E
	movb	%al, 31(%rsp)
	jmp	.LBB131_7
.LBB131_7:
	movq	48(%rsp), %rax
	movq	(%rax), %rcx
	cmpq	8(%rax), %rcx
	jne	.LBB131_9
	movq	$0, 56(%rsp)
	jmp	.LBB131_14
.LBB131_9:
	callq	_ZN4core3mem7size_of17h74ed6d9378771763E
	cmpq	$0, %rax
	jne	.LBB131_11
	movq	48(%rsp), %rax
	movq	8(%rax), %rdi
	movq	$-1, %rsi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$15wrapping_offset17ha9019008d988c570E
	movq	48(%rsp), %rsi
	movq	%rax, 8(%rsi)
	movq	(%rsi), %rax
	movq	%rax, 64(%rsp)
	jmp	.LBB131_12
.LBB131_11:
	movl	$1, %eax
	movl	%eax, %esi
	movq	48(%rsp), %rcx
	movq	(%rcx), %rdx
	movq	(%rcx), %rdi
	movq	%rdx, 16(%rsp)
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$6offset17h37e547802c70e09aE
	movq	48(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	16(%rsp), %rax
	movq	%rax, 64(%rsp)
.LBB131_12:
	movq	64(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, 56(%rsp)
.LBB131_14:
	movq	56(%rsp), %rax
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end131:
	.size	_ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE, .Lfunc_end131-_ZN91_$LT$core..slice..Iter$LT$$u27$a$C$$u20$T$GT$$u20$as$u20$core..iter..iterator..Iterator$GT$4next17hb4305562de5274cbE
	.cfi_endproc

	.section	".text._ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE,@function
_ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rdi
	callq	*_ZN3std3sys4unix5mutex14ReentrantMutex7destroy17h7f0d9b119445c505E@GOTPCREL(%rip)
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end132:
	.size	_ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE, .Lfunc_end132-_ZN91_$LT$std..sys_common..remutex..ReentrantMutex$LT$T$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17h3a8d1cf3dd70d3cfE
	.cfi_endproc

	.section	".text._ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE,@function
_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	$2, 8(%rsp)
	movq	8(%rsp), %rax
	movq	16(%rsp), %rdx
	movq	%rdi, (%rsp)
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end133:
	.size	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE, .Lfunc_end133-_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$11start_bound17h7931f9016c4c16aaE
	.cfi_endproc

	.section	".text._ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE,@function
_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, 8(%rsp)
	movq	$1, (%rsp)
	movq	(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end134:
	.size	_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE, .Lfunc_end134-_ZN93_$LT$core..ops..range..RangeTo$LT$T$GT$$u20$as$u20$core..ops..range..RangeBounds$LT$T$GT$$GT$9end_bound17ha2fcda45679663ffE
	.cfi_endproc

	.section	".text._ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E,@function
_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E:
	.cfi_startproc
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 48(%rsp)
	movq	%rdx, %rdi
	movq	%rsi, 40(%rsp)
	movq	%rcx, %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h14bf0abab807ad4cE
	movq	%rax, 32(%rsp)
	movq	32(%rsp), %rdi
	movq	48(%rsp), %rsi
	callq	_ZN4core3ptr33_$LT$impl$u20$$BP$const$u20$T$GT$3add17h5f76ac0e36a01ffcE
	movq	%rax, 24(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	subq	%rcx, %rax
	movq	24(%rsp), %rdi
	movq	%rax, %rsi
	callq	_ZN4core5slice14from_raw_parts17h22690902781b2c88E
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end135:
	.size	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E, .Lfunc_end135-_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E
	.cfi_endproc

	.section	".text._ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE,@function
_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE:
	.cfi_startproc
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	cmpq	%rsi, %rdi
	movq	%rdx, 64(%rsp)
	movq	%rsi, 56(%rsp)
	movq	%rdi, 48(%rsp)
	movq	%rcx, 40(%rsp)
	jbe	.LBB136_2
	movq	_ZN4core5slice22slice_index_order_fail17h0b6431721acd382eE@GOTPCREL(%rip), %rax
	movq	48(%rsp), %rdi
	movq	56(%rsp), %rsi
	callq	*%rax
	ud2
.LBB136_2:
	movq	64(%rsp), %rdi
	movq	40(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 32(%rsp)
	movq	56(%rsp), %rax
	movq	32(%rsp), %rcx
	cmpq	%rcx, %rax
	jbe	.LBB136_5
	movq	64(%rsp), %rdi
	movq	40(%rsp), %rsi
	callq	_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$3len17h57755f740fd5e094E
	movq	%rax, 24(%rsp)
	jmp	.LBB136_6
.LBB136_5:
	movq	48(%rsp), %rdi
	movq	56(%rsp), %rsi
	movq	64(%rsp), %rdx
	movq	40(%rsp), %rcx
	callq	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$13get_unchecked17h5bb4b93469758f60E
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	jmp	.LBB136_7
.LBB136_6:
	movq	_ZN4core5slice20slice_index_len_fail17h48cabcb158ba01dcE@GOTPCREL(%rip), %rax
	movq	56(%rsp), %rdi
	movq	24(%rsp), %rsi
	callq	*%rax
	ud2
.LBB136_7:
	movq	16(%rsp), %rax
	movq	8(%rsp), %rdx
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end136:
	.size	_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE, .Lfunc_end136-_ZN99_$LT$core..ops..range..Range$LT$usize$GT$$u20$as$u20$core..slice..SliceIndex$LT$$u5b$T$u5d$$GT$$GT$5index17h03a024c541b36e3fE
	.cfi_endproc

	.section	".text._ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E","ax",@progbits
	.p2align	4, 0x90
	.type	_ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E,@function
_ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E:
	.cfi_startproc
	subq	$16, %rsp
	.cfi_def_cfa_offset 24
	movq	%rdi, 8(%rsp)
	movq	8(%rsp), %rdi
	movq	%rdi, (%rsp)
	movq	(%rsp), %rax
	addq	$16, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end137:
	.size	_ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E, .Lfunc_end137-_ZN99_$LT$core..ptr..NonNull$LT$T$GT$$u20$as$u20$core..convert..From$LT$$RF$$u27$a$u20$mut$u20$T$GT$$GT$4from17h448a0deb526008c1E
	.cfi_endproc

	.section	.text.system_in_read,"ax",@progbits
	.globl	system_in_read
	.p2align	4, 0x90
	.type	system_in_read,@function
system_in_read:
	.cfi_startproc
	xorl	%eax, %eax
	retq
.Lfunc_end138:
	.size	system_in_read, .Lfunc_end138-system_in_read
	.cfi_endproc

	.section	.text.system_out_println,"ax",@progbits
	.globl	system_out_println
	.p2align	4, 0x90
	.type	system_out_println,@function
system_out_println:
	.cfi_startproc
	subq	$120, %rsp
	.cfi_def_cfa_offset 128
	movq	_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..Display$u20$for$u20$i32$GT$3fmt17h97845b111d1bbc8dE@GOTPCREL(%rip), %rsi
	movl	%edi, 44(%rsp)
	leaq	44(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	112(%rsp), %rdi
	callq	_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E
	movq	%rax, 32(%rsp)
	movq	%rdx, 24(%rsp)
	leaq	.L__unnamed_12(%rip), %rax
	movl	$2, %ecx
	movl	%ecx, %edx
	movl	$1, %ecx
	movl	%ecx, %r8d
	leaq	.L__unnamed_13(%rip), %rsi
	movq	32(%rsp), %rdi
	movq	%rdi, 96(%rsp)
	movq	24(%rsp), %r9
	movq	%r9, 104(%rsp)
	leaq	96(%rsp), %r10
	leaq	48(%rsp), %rdi
	movq	%rsi, 16(%rsp)
	movq	%rax, %rsi
	movq	%r10, %rcx
	movq	16(%rsp), %r9
	movq	$1, (%rsp)
	callq	_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE
	leaq	48(%rsp), %rdi
	callq	*_ZN3std2io5stdio6_print17h660907b119250544E@GOTPCREL(%rip)
	addq	$120, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end139:
	.size	system_out_println, .Lfunc_end139-system_out_println
	.cfi_endproc

	.section	.text.system_out_write,"ax",@progbits
	.globl	system_out_write
	.p2align	4, 0x90
	.type	system_out_write,@function
system_out_write:
	.cfi_startproc
	subq	$120, %rsp
	.cfi_def_cfa_offset 128
	movq	_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..Display$u20$for$u20$i32$GT$3fmt17h97845b111d1bbc8dE@GOTPCREL(%rip), %rsi
	movl	%edi, 44(%rsp)
	leaq	44(%rsp), %rax
	movq	%rax, 112(%rsp)
	movq	112(%rsp), %rdi
	callq	_ZN4core3fmt10ArgumentV13new17h362fd1c495976b37E
	movq	%rax, 32(%rsp)
	movq	%rdx, 24(%rsp)
	leaq	.L__unnamed_12(%rip), %rax
	movl	$2, %ecx
	movl	%ecx, %edx
	movl	$1, %ecx
	movl	%ecx, %r8d
	leaq	.L__unnamed_13(%rip), %rsi
	movq	32(%rsp), %rdi
	movq	%rdi, 96(%rsp)
	movq	24(%rsp), %r9
	movq	%r9, 104(%rsp)
	leaq	96(%rsp), %r10
	leaq	48(%rsp), %rdi
	movq	%rsi, 16(%rsp)
	movq	%rax, %rsi
	movq	%r10, %rcx
	movq	16(%rsp), %r9
	movq	$1, (%rsp)
	callq	_ZN4core3fmt9Arguments16new_v1_formatted17h2949aeb0bd5a8e0eE
	leaq	48(%rsp), %rdi
	callq	*_ZN3std2io5stdio6_print17h660907b119250544E@GOTPCREL(%rip)
	addq	$120, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end140:
	.size	system_out_write, .Lfunc_end140-system_out_write
	.cfi_endproc

	.section	.text.system_out_flush,"ax",@progbits
	.globl	system_out_flush
	.p2align	4, 0x90
	.type	system_out_flush,@function
system_out_flush:
.Lfunc_begin18:
	.cfi_startproc
	.cfi_personality 155, DW.ref.rust_eh_personality
	.cfi_lsda 27, .Lexception18
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	callq	*_ZN3std2io5stdio6stdout17hce49a00011d3f12aE@GOTPCREL(%rip)
	movq	%rax, 32(%rsp)
	jmp	.LBB141_2
.LBB141_1:
	movq	40(%rsp), %rdi
	callq	_Unwind_Resume@PLT
	ud2
.LBB141_2:
.Ltmp130:
	movq	_ZN57_$LT$std..io..stdio..Stdout$u20$as$u20$std..io..Write$GT$5flush17h7b9d26e04525f9bbE@GOTPCREL(%rip), %rax
	leaq	16(%rsp), %rdi
	leaq	32(%rsp), %rsi
	callq	*%rax
.Ltmp131:
	jmp	.LBB141_3
.LBB141_3:
.Ltmp132:
	leaq	16(%rsp), %rdi
	callq	_ZN47_$LT$core..result..Result$LT$T$C$$u20$E$GT$$GT$2ok17h4f4614000767dc96E
.Ltmp133:
	movb	%al, 15(%rsp)
	jmp	.LBB141_5
.LBB141_4:
	leaq	32(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E
	jmp	.LBB141_1
.LBB141_5:
	leaq	32(%rsp), %rdi
	callq	_ZN4core3ptr13drop_in_place17h7bbf834b7cfe8620E
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB141_7:
	.cfi_def_cfa_offset 64
.Ltmp134:
	movl	%edx, %ecx
	movq	%rax, 40(%rsp)
	movl	%ecx, 48(%rsp)
	jmp	.LBB141_4
.Lfunc_end141:
	.size	system_out_flush, .Lfunc_end141-system_out_flush
	.cfi_endproc
	.section	.gcc_except_table,"a",@progbits
	.p2align	2
GCC_except_table141:
.Lexception18:
	.byte	255
	.byte	255
	.byte	1
	.uleb128 .Lcst_end18-.Lcst_begin18
.Lcst_begin18:
	.uleb128 .Lfunc_begin18-.Lfunc_begin18
	.uleb128 .Ltmp130-.Lfunc_begin18
	.byte	0
	.byte	0
	.uleb128 .Ltmp130-.Lfunc_begin18
	.uleb128 .Ltmp133-.Ltmp130
	.uleb128 .Ltmp134-.Lfunc_begin18
	.byte	0
	.uleb128 .Ltmp133-.Lfunc_begin18
	.uleb128 .Lfunc_end141-.Ltmp133
	.byte	0
	.byte	0
.Lcst_end18:
	.p2align	2

	.section	.text._ZN4main4main17hd31e54a560abace9E,"ax",@progbits
	.p2align	4, 0x90
	.type	_ZN4main4main17hd31e54a560abace9E,@function
_ZN4main4main17hd31e54a560abace9E:
	.cfi_startproc
	subq	$120, %rsp
	.cfi_def_cfa_offset 128
	leaq	.L__unnamed_14(%rip), %rax
	movl	$1, %ecx
	movl	%ecx, %edx
	leaq	.L__unnamed_15(%rip), %rsi
	xorl	%ecx, %ecx
	movl	%ecx, %r8d
	leaq	24(%rsp), %rdi
	movq	%rsi, 16(%rsp)
	movq	%rax, %rsi
	movq	16(%rsp), %rcx
	callq	_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE
	leaq	24(%rsp), %rdi
	callq	*_ZN3std2io5stdio6_print17h660907b119250544E@GOTPCREL(%rip)
	callq	*mj_main@GOTPCREL(%rip)
	leaq	.L__unnamed_16(%rip), %rax
	movl	$1, %ecx
	movl	%ecx, %edx
	leaq	.L__unnamed_15(%rip), %rsi
	xorl	%ecx, %ecx
	movl	%ecx, %r8d
	leaq	72(%rsp), %rdi
	movq	%rsi, 8(%rsp)
	movq	%rax, %rsi
	movq	8(%rsp), %rcx
	callq	_ZN4core3fmt9Arguments6new_v117h5089d44b39e0b9adE
	leaq	72(%rsp), %rdi
	callq	*_ZN3std2io5stdio6_print17h660907b119250544E@GOTPCREL(%rip)
	addq	$120, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end142:
	.size	_ZN4main4main17hd31e54a560abace9E, .Lfunc_end142-_ZN4main4main17hd31e54a560abace9E
	.cfi_endproc

	.section	.text.main,"ax",@progbits
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movslq	%edi, %rax
	leaq	_ZN4main4main17hd31e54a560abace9E(%rip), %rdi
	movq	%rsi, (%rsp)
	movq	%rax, %rsi
	movq	(%rsp), %rdx
	callq	_ZN3std2rt10lang_start17hf4574541ae2d563fE
	movl	%eax, %ecx
	movl	%ecx, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end143:
	.size	main, .Lfunc_end143-main
	.cfi_endproc

	.type	.L__unnamed_17,@object
	.section	.rodata..L__unnamed_17,"a",@progbits
.L__unnamed_17:
	.ascii	"assertion failed: start <= end"
	.size	.L__unnamed_17, 30

	.type	.L__unnamed_18,@object
	.section	.rodata..L__unnamed_18,"a",@progbits
.L__unnamed_18:
	.ascii	"liballoc/vec.rs"
	.size	.L__unnamed_18, 15

	.type	.L__unnamed_1,@object
	.section	.data.rel.ro..L__unnamed_1,"aw",@progbits
	.p2align	3
.L__unnamed_1:
	.quad	.L__unnamed_17
	.asciz	"\036\000\000\000\000\000\000"
	.quad	.L__unnamed_18
	.asciz	"\017\000\000\000\000\000\000\000^\004\000\000\t\000\000"
	.size	.L__unnamed_1, 40

	.type	.L__unnamed_19,@object
	.section	.rodata..L__unnamed_19,"a",@progbits
.L__unnamed_19:
	.ascii	"assertion failed: end <= len"
	.size	.L__unnamed_19, 28

	.type	.L__unnamed_2,@object
	.section	.data.rel.ro..L__unnamed_2,"aw",@progbits
	.p2align	3
.L__unnamed_2:
	.quad	.L__unnamed_19
	.asciz	"\034\000\000\000\000\000\000"
	.quad	.L__unnamed_18
	.asciz	"\017\000\000\000\000\000\000\000_\004\000\000\t\000\000"
	.size	.L__unnamed_2, 40

	.type	.L__unnamed_20,@object
	.section	.rodata..L__unnamed_20,"a",@progbits
.L__unnamed_20:
	.ascii	"called `Option::unwrap()` on a `None` value"
	.size	.L__unnamed_20, 43

	.type	.L__unnamed_21,@object
	.section	.rodata..L__unnamed_21,"a",@progbits
.L__unnamed_21:
	.ascii	"libcore/option.rs"
	.size	.L__unnamed_21, 17

	.type	.L__unnamed_3,@object
	.section	.data.rel.ro..L__unnamed_3,"aw",@progbits
	.p2align	3
.L__unnamed_3:
	.quad	.L__unnamed_20
	.asciz	"+\000\000\000\000\000\000"
	.quad	.L__unnamed_21
	.asciz	"\021\000\000\000\000\000\000\000c\001\000\000\025\000\000"
	.size	.L__unnamed_3, 40

	.type	.L__unnamed_4,@object
	.section	.data.rel.ro..L__unnamed_4,"aw",@progbits
	.p2align	3
.L__unnamed_4:
	.quad	_ZN4core3ptr13drop_in_place17ha263f64279284448E
	.quad	8
	.quad	8
	.quad	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E
	.quad	_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17hff046c42699dcec6E
	.quad	_ZN4core3ops8function6FnOnce9call_once17hb24f94347ae3c72cE
	.size	.L__unnamed_4, 48

	.type	.L__unnamed_6,@object
	.section	.rodata..L__unnamed_6,"a",@progbits
.L__unnamed_6:
	.byte	15
	.size	.L__unnamed_6, 1

	.type	.L__unnamed_5,@object
	.section	.rodata..L__unnamed_5,"a",@progbits
.L__unnamed_5:
	.ascii	"failed to write the buffered data"
	.size	.L__unnamed_5, 33

	.type	.L__unnamed_7,@object
	.section	.rodata..L__unnamed_7,"a",@progbits
.L__unnamed_7:
	.ascii	"called `Result::unwrap()` on an `Err` value"
	.size	.L__unnamed_7, 43

	.type	.L__unnamed_22,@object
	.section	.rodata..L__unnamed_22,"a",@progbits
.L__unnamed_22:
	.ascii	"there is no such thing as a relaxed fence"
	.size	.L__unnamed_22, 41

	.type	.L__unnamed_23,@object
	.section	.rodata..L__unnamed_23,"a",@progbits
.L__unnamed_23:
	.ascii	"libcore/sync/atomic.rs"
	.size	.L__unnamed_23, 22

	.type	.L__unnamed_8,@object
	.section	.data.rel.ro..L__unnamed_8,"aw",@progbits
	.p2align	3
.L__unnamed_8:
	.quad	.L__unnamed_22
	.asciz	")\000\000\000\000\000\000"
	.quad	.L__unnamed_23
	.asciz	"\026\000\000\000\000\000\000\000\335\b\000\000\030\000\000"
	.size	.L__unnamed_8, 40

	.type	.L__unnamed_15,@object
	.section	.rodata..L__unnamed_15,"a",@progbits
	.p2align	3
.L__unnamed_15:
	.size	.L__unnamed_15, 0

	.type	.L__unnamed_24,@object
	.section	.rodata..L__unnamed_24,"a",@progbits
.L__unnamed_24:
	.ascii	": "
	.size	.L__unnamed_24, 2

	.type	.L__unnamed_9,@object
	.section	.data.rel.ro..L__unnamed_9,"aw",@progbits
	.p2align	3
.L__unnamed_9:
	.quad	.L__unnamed_15
	.zero	8
	.quad	.L__unnamed_24
	.asciz	"\002\000\000\000\000\000\000"
	.size	.L__unnamed_9, 32

	.type	.L__unnamed_10,@object
	.section	.rodata..L__unnamed_10,"a",@progbits
	.p2align	3
.L__unnamed_10:
	.asciz	"\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\003\000\000\000\000\000\000"
	.size	.L__unnamed_10, 128

	.type	.L__unnamed_25,@object
	.section	.rodata..L__unnamed_25,"a",@progbits
.L__unnamed_25:
	.ascii	"libcore/result.rs"
	.size	.L__unnamed_25, 17

	.type	.L__unnamed_11,@object
	.section	.data.rel.ro..L__unnamed_11,"aw",@progbits
	.p2align	3
.L__unnamed_11:
	.quad	.L__unnamed_25
	.asciz	"\021\000\000\000\000\000\000\000\361\003\000\000\005\000\000"
	.size	.L__unnamed_11, 24

	.type	.L__unnamed_26,@object
	.section	.rodata..L__unnamed_26,"a",@progbits
.L__unnamed_26:
	.byte	10
	.size	.L__unnamed_26, 1

	.type	.L__unnamed_12,@object
	.section	.data.rel.ro..L__unnamed_12,"aw",@progbits
	.p2align	3
.L__unnamed_12:
	.quad	.L__unnamed_15
	.zero	8
	.quad	.L__unnamed_26
	.asciz	"\001\000\000\000\000\000\000"
	.size	.L__unnamed_12, 32

	.type	.L__unnamed_13,@object
	.section	.rodata..L__unnamed_13,"a",@progbits
	.p2align	3
.L__unnamed_13:
	.asciz	"\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\003\000\000\000\000\000\000"
	.size	.L__unnamed_13, 64

	.type	.L__unnamed_27,@object
	.section	.rodata..L__unnamed_27,"a",@progbits
.L__unnamed_27:
	.ascii	"Hello, world!\n"
	.size	.L__unnamed_27, 14

	.type	.L__unnamed_14,@object
	.section	.data.rel.ro..L__unnamed_14,"aw",@progbits
	.p2align	3
.L__unnamed_14:
	.quad	.L__unnamed_27
	.asciz	"\016\000\000\000\000\000\000"
	.size	.L__unnamed_14, 16

	.type	.L__unnamed_28,@object
	.section	.rodata..L__unnamed_28,"a",@progbits
.L__unnamed_28:
	.ascii	"MJ Main, ran!\n"
	.size	.L__unnamed_28, 14

	.type	.L__unnamed_16,@object
	.section	.data.rel.ro..L__unnamed_16,"aw",@progbits
	.p2align	3
.L__unnamed_16:
	.quad	.L__unnamed_28
	.asciz	"\016\000\000\000\000\000\000"
	.size	.L__unnamed_16, 16

	.hidden	DW.ref.rust_eh_personality
	.weak	DW.ref.rust_eh_personality
	.section	.data.DW.ref.rust_eh_personality,"aGw",@progbits,DW.ref.rust_eh_personality,comdat
	.p2align	3
	.type	DW.ref.rust_eh_personality,@object
	.size	DW.ref.rust_eh_personality, 8
DW.ref.rust_eh_personality:
	.quad	rust_eh_personality

	.section	".note.GNU-stack","",@progbits
