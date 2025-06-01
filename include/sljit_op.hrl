-ifndef(__SLJIT_OP_HRL__).
-define(__SLJIT_OP_HRL__, true).

-define(SLJIT_ARG_TYPE_SCRATCH_REG, 16#8).
-define(SLJIT_ARG_TYPE_RET_VOID,	0).
-define(SLJIT_ARG_TYPE_W,	1).
-define(SLJIT_ARG_TYPE_W_R,	(?SLJIT_ARG_TYPE_W bor ?SLJIT_ARG_TYPE_SCRATCH_REG)).
-define(SLJIT_ARG_TYPE_32,	2).
-define(SLJIT_ARG_TYPE_32_R,	(?SLJIT_ARG_TYPE_32 bor ?SLJIT_ARG_TYPE_SCRATCH_REG)).
-define(SLJIT_ARG_TYPE_P,	3).
-define(SLJIT_ARG_TYPE_P_R,	(?SLJIT_ARG_TYPE_P bor ?SLJIT_ARG_TYPE_SCRATCH_REG)).
-define(SLJIT_ARG_TYPE_F64,	4).
-define(SLJIT_ARG_TYPE_F32,	5).
%% erlang extension
-define(SLJIT_ARG_TYPE_TERM,	6).
-define(SLJIT_ARG_TYPE_TERM_R,	(?SLJIT_ARG_TYPE_TERM bor ?SLJIT_ARG_TYPE_SCRATCH_REG)).

-define(SLJIT_ARG_SHIFT, 4).
-define(SLJIT_ARG_RETURN(Type), (Type)).
-define(SLJIT_ARG_VALUE(Type,Idx), ((Type) bsl ((Idx) * ?SLJIT_ARG_SHIFT))).
%% -define(SLJIT_ARG_TO_TYPE(Type), (?SLJIT_ARG_TYPE_ ??Type)).
%% -define(SLJIT_ARGS0(Ret), ?SLJIT_ARG_RETURN(?SLJIT_ARG_TO_TYPE(Ret))).
-define(SLJIT_ARGS0(Ret), ?SLJIT_ARG_RETURN(Ret)).
-define(SLJIT_ARGS0V(), ?SLJIT_ARG_RETURN(?SLJIT_ARG_TYPE_RET_VOID)

-define(SLJIT_ARGS1(Ret, Arg1), (?SLJIT_ARGS0(Ret) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg1), 1))).
-define(SLJIT_ARGS1V(Arg1), (?SLJIT_ARGS0V() bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg1), 1))).
-define(SLJIT_ARGS2(Ret, Arg1, Arg2), (?SLJIT_ARGS1(Ret, Arg1) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg2), 2))).
-define(SLJIT_ARGS2V(Arg1, Arg2), (?SLJIT_ARGS1V(Arg1) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg2), 2))).
-define(SLJIT_ARGS3(Ret, Arg1, Arg2, Arg3), (?SLJIT_ARGS2(Ret, Arg1, Arg2) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg3), 3))).
-define(SLJIT_ARGS3V(Arg1, Arg2, Arg3), (?SLJIT_ARGS2V(Arg1, Arg2) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg3), 3))).
-define(SLJIT_ARGS4(Ret, Arg1, Arg2, Arg3, Arg4), (?SLJIT_ARGS3(Ret, Arg1, Arg2, Arg3) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg4), 4))).
-define(SLJIT_ARGS4V(Arg1, Arg2, Arg3, Arg4), (?SLJIT_ARGS3V(Arg1, Arg2, Arg3) bor ?SLJIT_ARG_VALUE(?SLJIT_ARG_TO_TYPE(Arg4), 4))).

-define(SLJIT_HAS_FPU,			0).
-define(SLJIT_HAS_VIRTUAL_REGISTERS,	1).
-define(SLJIT_HAS_ZERO_REGISTER,		2).
-define(SLJIT_HAS_CLZ,			3).
-define(SLJIT_HAS_CTZ,			4).
-define(SLJIT_HAS_REV,			5).
-define(SLJIT_HAS_ROT,			6).
-define(SLJIT_HAS_CMOV,			7).
-define(SLJIT_HAS_PREFETCH,		8).
-define(SLJIT_HAS_COPY_F32,		9).
-define(SLJIT_HAS_COPY_F64,		10).
-define(SLJIT_HAS_F64_AS_F32_PAIR,	11).
-define(SLJIT_HAS_SIMD,			12).
-define(SLJIT_SIMD_REGS_ARE_PAIRS,	13).
-define(SLJIT_HAS_ATOMIC,		14).
-define(SLJIT_HAS_MEMORY_BARRIER,	15).
-define(SLJIT_HAS_AVX,			100).
-define(SLJIT_HAS_AVX2,			101).
-define(SLJIT_HAS_LASX,                 201).


-define(SLJIT_ENTER_KEEP(N),		(N)).  %% 1..3
-define(SLJIT_ENTER_REG_ARG,		16#00000004).
-define(SLJIT_ENTER_USE_VEX,		16#00010000).

-define(SLJIT_ENTER_FLOAT(Regs),	((Regs) bsl 8)).
-define(SLJIT_ENTER_VECTOR(Regs),	((Regs) bsl 16)).
-define(SLJIT_MAX_LOCAL_SIZE,		1048576).
-define(SLJIT_MEM,		16#80).
-define(SLJIT_MEM0(),		(?SLJIT_MEM)).
-define(SLJIT_MEM1(R1),		(?SLJIT_MEM bor (R1))).
-define(SLJIT_MEM2(R1,R2),	(?SLJIT_MEM bor (R1) bor ((R2) bsl 8))).
-define(SLJIT_IMM,		16#7f).
-define(SLJIT_REG_PAIR(R1,R2),	((R1) bor ((R2) bsl 8))).
-define(SLJIT_IS_REG(Arg),	(((Arg) > 0) andalso ((Arg) < ?SLJIT_IMM))).
-define(SLJIT_IS_MEM(Arg),	((Arg) band ?SLJIT_MEM)).
-define(SLJIT_IS_MEM0(Arg),	((Arg) == ?SLJIT_MEM)).
-define(SLJIT_IS_MEM1(Arg),	((Arg) > ?SLJIT_MEM andalso (Arg) < (?SLJIT_MEM bsl 1))).
-define(SLJIT_IS_MEM2(Arg),	(((Arg) band ?SLJIT_MEM) andalso (Arg) >= (?SLJIT_MEM bsl 1))).
-define(SLJIT_IS_IMM(Arg),	((Arg) =:= ?SLJIT_IMM)).
-define(SLJIT_IS_REG_PAIR(Arg),	(not ((Arg) band ?SLJIT_MEM) andalso (Arg) >= (?SLJIT_MEM bsl 1))).
-define(SLJIT_EXTRACT_REG(Arg),		((Arg) band 16#7f)).
-define(SLJIT_EXTRACT_SECOND_REG(Arg),	((Arg) bsr 8)).

-endif.
