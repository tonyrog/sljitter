-ifndef(__SLJIT_OP_HRL__).
-define(__SLJIT_OP_HRL__, true).

%%
%% config FIXME!!! MUST lookup/updated with sljitConfig.h (fixme)
%% cat sljitConfigCPU.h sljitConfigInternal.h > const1.h
%% gcc -dM -E const.h | grep "#define SLJIT_NUMBER_OF_" > const2.h
%% 
%% X86_64 !!!
-define(SLJIT_NUMBER_OF_REGISTERS, 13).
-define(SLJIT_NUMBER_OF_TEMPORARY_REGISTERS, 2).
-define(SLJIT_NUMBER_OF_FLOAT_REGISTERS, 15).
-define(SLJIT_NUMBER_OF_TEMPORARY_FLOAT_REGISTERS, 1).
-define(SLJIT_NUMBER_OF_SAVED_REGISTERS, 6).
-define(SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS, 0).
-define(SLJIT_LOCALS_OFFSET_BASE, 0).

-define(SLJIT_NUMBER_OF_VECTOR_REGISTERS, 15).
-define(SLJIT_NUMBER_OF_SAVED_VECTOR_REGISTERS, 15).

-define(SLJIT_TMP_DEST_REG, ?SLJIT_TMP_R0).
-define(SLJIT_TMP_MEM_REG, ?SLJIT_TMP_R0).
-define(SLJIT_TMP_DEST_FREG, ?SLJIT_TMP_FR0).
-define(SLJIT_PREF_SHIFT_REG, ?SLJIT_R3).
-define(SLJIT_MASKED_SHIFT, 1).
-define(SLJIT_MASKED_SHIFT32, 1).
-define(SLJIT_UPPER_BITS_IGNORED, 1).
-define(SLJIT_UPPER_BITS_ZERO_EXTENDED, 1).

-define(SLJIT_R0,	1).
-define(SLJIT_R1,	2).
-define(SLJIT_R2,	3).
-define(SLJIT_R3,	4).
-define(SLJIT_R4,	5).
-define(SLJIT_R5,	6).
-define(SLJIT_R6,	7).
-define(SLJIT_R7,	8).
-define(SLJIT_R8,	9).
-define(SLJIT_R9,	10).
-define(SLJIT_R(I),	(1 + (I))).
-define(SLJIT_S0,	(?SLJIT_NUMBER_OF_REGISTERS)).
-define(SLJIT_S1,	(?SLJIT_NUMBER_OF_REGISTERS - 1)).
-define(SLJIT_S2,	(?SLJIT_NUMBER_OF_REGISTERS - 2)).
-define(SLJIT_S3,	(?SLJIT_NUMBER_OF_REGISTERS - 3)).
-define(SLJIT_S4,	(?SLJIT_NUMBER_OF_REGISTERS - 4)).
-define(SLJIT_S5,	(?SLJIT_NUMBER_OF_REGISTERS - 5)).
-define(SLJIT_S6,	(?SLJIT_NUMBER_OF_REGISTERS - 6)).
-define(SLJIT_S7,	(?SLJIT_NUMBER_OF_REGISTERS - 7)).
-define(SLJIT_S8,	(?SLJIT_NUMBER_OF_REGISTERS - 8)).
-define(SLJIT_S9,	(?SLJIT_NUMBER_OF_REGISTERS - 9)).
-define(SLJIT_S(I),	(?SLJIT_NUMBER_OF_REGISTERS - (I))).
-define(SLJIT_FIRST_SAVED_REG, (?SLJIT_S0 - ?SLJIT_NUMBER_OF_SAVED_REGISTERS + 1)).
-define(SLJIT_SP,	(?SLJIT_NUMBER_OF_REGISTERS + 1)).
-define(SLJIT_RETURN_REG, ?SLJIT_R0).

-define(SLJIT_FR0,	1).
-define(SLJIT_FR1,	2).
-define(SLJIT_FR2,	3).
-define(SLJIT_FR3,	4).
-define(SLJIT_FR4,	5).
-define(SLJIT_FR5,	6).
-define(SLJIT_FR6,	7).
-define(SLJIT_FR7,	8).
-define(SLJIT_FR8,	9).
-define(SLJIT_FR9,	10).
-define(SLJIT_FR(I),	(1 + (I))).

-define(SLJIT_FS0,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS)).
-define(SLJIT_FS1,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 1)).
-define(SLJIT_FS2,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 2)).
-define(SLJIT_FS3,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 3)).
-define(SLJIT_FS4,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 4)).
-define(SLJIT_FS5,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 5)).
-define(SLJIT_FS6,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 6)).
-define(SLJIT_FS7,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 7)).
-define(SLJIT_FS8,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 8)).
-define(SLJIT_FS9,	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - 9)).
-define(SLJIT_FS(I),	(?SLJIT_NUMBER_OF_FLOAT_REGISTERS - (I))).

-define(SLJIT_FIRST_SAVED_FLOAT_REG, (?SLJIT_FS0 - ?SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS + 1)).
-define(SLJIT_RETURN_FREG,	?SLJIT_FR0).

-define(SLJIT_VR0,	1).
-define(SLJIT_VR1,	2).
-define(SLJIT_VR2,	3).
-define(SLJIT_VR3,	4).
-define(SLJIT_VR4,	5).
-define(SLJIT_VR5,	6).
-define(SLJIT_VR6,	7).
-define(SLJIT_VR7,	8).
-define(SLJIT_VR8,	9).
-define(SLJIT_VR9,	10).
-define(SLJIT_VR(I),	(1 + (I))).

-define(SLJIT_VS0,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS)).
-define(SLJIT_VS1,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 1)).
-define(SLJIT_VS2,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 2)).
-define(SLJIT_VS3,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 3)).
-define(SLJIT_VS4,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 4)).
-define(SLJIT_VS5,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 5)).
-define(SLJIT_VS6,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 6)).
-define(SLJIT_VS7,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 7)).
-define(SLJIT_VS8,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 8)).
-define(SLJIT_VS9,	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - 9)).
-define(SLJIT_VS(I),	(?SLJIT_NUMBER_OF_VECTOR_REGISTERS - (I))).
-define(SLJIT_FIRST_SAVED_VECTOR_REG, (?SLJIT_VS0 - ?SLJIT_NUMBER_OF_SAVED_VECTOR_REGISTERS + 1)).
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


-define(SLJIT_ENTER_KEEP(N),		(N)).
-define(SLJIT_ENTER_REG_ARG,		16#00000004).
-define(SLJIT_ENTER_USE_VEX,		1600010000).

-define(SLJIT_ENTER_FLOAT(Regs),	((Regs) bsl 8)).
-define(SLJIT_ENTER_VECTOR(Regs),	((Regs) bsl 16)).
-define(SLJIT_MAX_LOCAL_SIZE,		1048576).
-define(SLJIT_MEM,		16#80).
-define(SLJIT_MEM0(),		(?SLJIT_MEM)).
-define(SLJIT_MEM1(R1),		(?SLJIT_MEM bor (R1))).
-define(SLJIT_MEM2(R1,R2),	(?SLJIT_MEM bor (R1) bor ((R2) bsl 8))).
-define(SLJIT_IMM,		16#7f).
-define(SLJIT_REG_PAIR(R1,R2),	((R1) bor ((R2) bsl 8))).
-define(SLJIT_IS_REG(Arg),	((Arg) > 0 andalso (arg) < ?SLJIT_IMM)).
-define(SLJIT_IS_MEM(Arg),	((Arg) band ?SLJIT_MEM)).
-define(SLJIT_IS_MEM0(Arg),	((Arg) == ?SLJIT_MEM)).
-define(SLJIT_IS_MEM1(Arg),	((Arg) > ?SLJIT_MEM andalso (Arg) < (?SLJIT_MEM bsl 1))).
-define(SLJIT_IS_MEM2(Arg),	(((Arg) band ?SLJIT_MEM) andalso (Arg) >= (?SLJIT_MEM bsl 1))).
-define(SLJIT_IS_IMM(Arg),	((Arg) =:= ?SLJIT_IMM)).
-define(SLJIT_IS_REG_PAIR(Arg),	(not ((Arg) band ?SLJIT_MEM) andalso (Arg) >= (?SLJIT_MEM bsl 1))).
-define(SLJIT_EXTRACT_REG(Arg),		((Arg) band 16#7f)).
-define(SLJIT_EXTRACT_SECOND_REG(Arg),	((Arg) bsr 8)).

-endif.
