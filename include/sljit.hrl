-ifndef(__SLJIT_HRL__).
-define(__SLJIT_HRL__, true).

-include("sljit_op.hrl").

-define(SLJIT_32,		16#100).
-define(SLJIT_SET_Z,		16#0200).
-define(SLJIT_SET(Condition),	((Condition) bsl 10)).

-define(SLJIT_OP0_BASE,			0).
-define(SLJIT_BREAKPOINT,		(?SLJIT_OP0_BASE + 0)).
-define(SLJIT_NOP,			(?SLJIT_OP0_BASE + 1)).
-define(SLJIT_LMUL_UW,			(?SLJIT_OP0_BASE + 2)).
-define(SLJIT_LMUL_SW,			(?SLJIT_OP0_BASE + 3)).
-define(SLJIT_DIVMOD_UW,		(?SLJIT_OP0_BASE + 4)).
-define(SLJIT_DIVMOD_U32,		(?SLJIT_DIVMOD_UW bor ?SLJIT_32)).
-define(SLJIT_DIVMOD_SW,		(?SLJIT_OP0_BASE + 5)).
-define(SLJIT_DIVMOD_S32,		(?SLJIT_DIVMOD_SW bor ?SLJIT_32)).
-define(SLJIT_DIV_UW,			(?SLJIT_OP0_BASE + 6)).
-define(SLJIT_DIV_U32,			(?SLJIT_DIV_UW bor ?SLJIT_32)).
-define(SLJIT_DIV_SW,			(?SLJIT_OP0_BASE + 7)).
-define(SLJIT_DIV_S32,			(?SLJIT_DIV_SW bor ?SLJIT_32)).
-define(SLJIT_MEMORY_BARRIER,		(?SLJIT_OP0_BASE + 8)).
-define(SLJIT_ENDBR,			(?SLJIT_OP0_BASE + 9)).
-define(SLJIT_SKIP_FRAMES_BEFORE_RETURN,	(?SLJIT_OP0_BASE + 10)).

-define(SLJIT_OP1_BASE,			32).
-define(SLJIT_MOV,			(?SLJIT_OP1_BASE + 0)).
-define(SLJIT_MOV_U8,			(?SLJIT_OP1_BASE + 1)).
-define(SLJIT_MOV32_U8,			(?SLJIT_MOV_U8 bor ?SLJIT_32)).
-define(SLJIT_MOV_S8,			(?SLJIT_OP1_BASE + 2)).
-define(SLJIT_MOV32_S8,			(?SLJIT_MOV_S8 bor ?SLJIT_32)).
-define(SLJIT_MOV_U16,			(?SLJIT_OP1_BASE + 3)).
-define(SLJIT_MOV32_U16,		(?SLJIT_MOV_U16 bor ?SLJIT_32)).
-define(SLJIT_MOV_S16,			(?SLJIT_OP1_BASE + 4)).
-define(SLJIT_MOV32_S16,		(?SLJIT_MOV_S16 bor ?SLJIT_32)).
-define(SLJIT_MOV_U32,			(?SLJIT_OP1_BASE + 5)).
-define(SLJIT_MOV_S32,			(?SLJIT_OP1_BASE + 6)).
-define(SLJIT_MOV32,			(?SLJIT_OP1_BASE + 7)).
-define(SLJIT_MOV_P,			(?SLJIT_OP1_BASE + 8)).
-define(SLJIT_CLZ,			(?SLJIT_OP1_BASE + 9)).
-define(SLJIT_CLZ32,			(?SLJIT_CLZ bor ?SLJIT_32)).
-define(SLJIT_CTZ,			(?SLJIT_OP1_BASE + 10)).
-define(SLJIT_CTZ32,			(?SLJIT_CTZ bor ?SLJIT_32)).
-define(SLJIT_REV,			(?SLJIT_OP1_BASE + 11)).
-define(SLJIT_REV32,			(?SLJIT_REV bor ?SLJIT_32)).
-define(SLJIT_REV_U16,			(?SLJIT_OP1_BASE + 12)).
-define(SLJIT_REV32_U16,		(?SLJIT_REV_U16 bor ?SLJIT_32)).
-define(SLJIT_REV_S16,			(?SLJIT_OP1_BASE + 13)).
-define(SLJIT_REV32_S16,		(?SLJIT_REV_S16 bor ?SLJIT_32)).
-define(SLJIT_REV_U32,			(?SLJIT_OP1_BASE + 14)).
-define(SLJIT_REV_S32,			(?SLJIT_OP1_BASE + 15)).

-define(SLJIT_OP2_BASE,			64).
-define(SLJIT_ADD,			(?SLJIT_OP2_BASE + 0)).
-define(SLJIT_ADD32,			(?SLJIT_ADD bor ?SLJIT_32)).
-define(SLJIT_ADDC,			(?SLJIT_OP2_BASE + 1)).
-define(SLJIT_ADDC32,			(?SLJIT_ADDC bor ?SLJIT_32)).
-define(SLJIT_SUB,			(?SLJIT_OP2_BASE + 2)).
-define(SLJIT_SUB32,			(?SLJIT_SUB bor ?SLJIT_32)).
-define(SLJIT_SUBC,			(?SLJIT_OP2_BASE + 3)).
-define(SLJIT_SUBC32,			(?SLJIT_SUBC bor ?SLJIT_32)).
-define(SLJIT_MUL,			(?SLJIT_OP2_BASE + 4)).
-define(SLJIT_MUL32,			(?SLJIT_MUL bor ?SLJIT_32)).
-define(SLJIT_AND,			(?SLJIT_OP2_BASE + 5)).
-define(SLJIT_AND32,			(?SLJIT_AND bor ?SLJIT_32)).
-define(SLJIT_OR,			(?SLJIT_OP2_BASE + 6)).
-define(SLJIT_OR32,			(?SLJIT_OR bor ?SLJIT_32)).
-define(SLJIT_XOR,			(?SLJIT_OP2_BASE + 7)).
-define(SLJIT_XOR32,			(?SLJIT_XOR bor ?SLJIT_32)).
-define(SLJIT_SHL,			(?SLJIT_OP2_BASE + 8)).
-define(SLJIT_SHL32,			(?SLJIT_SHL bor ?SLJIT_32)).
-define(SLJIT_MSHL,			(?SLJIT_OP2_BASE + 9)).
-define(SLJIT_MSHL32,			(?SLJIT_MSHL bor ?SLJIT_32)).
-define(SLJIT_LSHR,			(?SLJIT_OP2_BASE + 10)).
-define(SLJIT_LSHR32,			(?SLJIT_LSHR bor ?SLJIT_32)).
-define(SLJIT_MLSHR,			(?SLJIT_OP2_BASE + 11)).
-define(SLJIT_MLSHR32,			(?SLJIT_MLSHR bor ?SLJIT_32)).
-define(SLJIT_ASHR,			(?SLJIT_OP2_BASE + 12)).
-define(SLJIT_ASHR32,			(?SLJIT_ASHR bor ?SLJIT_32)).
-define(SLJIT_MASHR,			(?SLJIT_OP2_BASE + 13)).
-define(SLJIT_MASHR32,			(?SLJIT_MASHR bor ?SLJIT_32)).
-define(SLJIT_ROTL,			(?SLJIT_OP2_BASE + 14)).
-define(SLJIT_ROTL32,			(?SLJIT_ROTL bor ?SLJIT_32)).
-define(SLJIT_ROTR,			(?SLJIT_OP2_BASE + 15)).
-define(SLJIT_ROTR32,			(?SLJIT_ROTR bor ?SLJIT_32)).

-define(SLJIT_OP2R_BASE,		96).
-define(SLJIT_MULADD,			(?SLJIT_OP2R_BASE + 0)).
-define(SLJIT_MULADD32,			(?SLJIT_MULADD bor ?SLJIT_32)).

-define(SLJIT_SHIFT_INTO_NON_ZERO,	16#200).

-define(SLJIT_OP_SRC_DST_BASE,		112).
-define(SLJIT_FAST_RETURN,		(?SLJIT_OP_SRC_DST_BASE + 0)).
-define(SLJIT_SKIP_FRAMES_BEFORE_FAST_RETURN,	(?SLJIT_OP_SRC_DST_BASE + 1)).
-define(SLJIT_PREFETCH_L1,		(?SLJIT_OP_SRC_DST_BASE + 2)).
-define(SLJIT_PREFETCH_L2,		(?SLJIT_OP_SRC_DST_BASE + 3)).
-define(SLJIT_PREFETCH_L3,		(?SLJIT_OP_SRC_DST_BASE + 4)).
-define(SLJIT_PREFETCH_ONCE,		(?SLJIT_OP_SRC_DST_BASE + 5)).
-define(SLJIT_FAST_ENTER,		(?SLJIT_OP_SRC_DST_BASE + 6)).
-define(SLJIT_GET_RETURN_ADDRESS,	(?SLJIT_OP_SRC_DST_BASE + 7)).

-define(SLJIT_FOP1_BASE,	      144).
-define(SLJIT_MOV_F64,		      (?SLJIT_FOP1_BASE + 0)).
-define(SLJIT_MOV_F32,		      (?SLJIT_MOV_F64 bor ?SLJIT_32)).
-define(SLJIT_CONV_F64_FROM_F32,      (?SLJIT_FOP1_BASE + 1)).
-define(SLJIT_CONV_F32_FROM_F64,      (?SLJIT_CONV_F64_FROM_F32 bor ?SLJIT_32)).

-define(SLJIT_CONV_SW_FROM_F64,	      (?SLJIT_FOP1_BASE + 2)).
-define(SLJIT_CONV_SW_FROM_F32,       (?SLJIT_CONV_SW_FROM_F64 bor ?SLJIT_32)).
-define(SLJIT_CONV_S32_FROM_F64,      (?SLJIT_FOP1_BASE + 3)).
-define(SLJIT_CONV_S32_FROM_F32,      (?SLJIT_CONV_S32_FROM_F64 bor ?SLJIT_32)).

-define(SLJIT_CONV_F64_FROM_SW,	      (?SLJIT_FOP1_BASE + 4)).
-define(SLJIT_CONV_F32_FROM_SW,	      (?SLJIT_CONV_F64_FROM_SW bor ?SLJIT_32)).

-define(SLJIT_CONV_F64_FROM_S32,      (?SLJIT_FOP1_BASE + 5)).
-define(SLJIT_CONV_F32_FROM_S32,      (?SLJIT_CONV_F64_FROM_S32 bor ?SLJIT_32)).

-define(SLJIT_CONV_F64_FROM_UW,	      (?SLJIT_FOP1_BASE + 6)).
-define(SLJIT_CONV_F32_FROM_UW,	      (?SLJIT_CONV_F64_FROM_UW bor ?SLJIT_32)).

-define(SLJIT_CONV_F64_FROM_U32,      (?SLJIT_FOP1_BASE + 7)).
-define(SLJIT_CONV_F32_FROM_U32,      (?SLJIT_CONV_F64_FROM_U32 bor ?SLJIT_32)).

-define(SLJIT_CMP_F64,		      (?SLJIT_FOP1_BASE + 8)).
-define(SLJIT_CMP_F32,		      (?SLJIT_CMP_F64 bor ?SLJIT_32)).

-define(SLJIT_NEG_F64,		      (?SLJIT_FOP1_BASE + 9)).
-define(SLJIT_NEG_F32,		      (?SLJIT_NEG_F64 bor ?SLJIT_32)).

-define(SLJIT_ABS_F64,		      (?SLJIT_FOP1_BASE + 10)).
-define(SLJIT_ABS_F32,		      (?SLJIT_ABS_F64 bor ?SLJIT_32)).

-define(SLJIT_FOP2_BASE,	      176).
-define(SLJIT_ADD_F64,			(?SLJIT_FOP2_BASE + 0)).
-define(SLJIT_ADD_F32,			(?SLJIT_ADD_F64 bor ?SLJIT_32)).
-define(SLJIT_SUB_F64,			(?SLJIT_FOP2_BASE + 1)).
-define(SLJIT_SUB_F32,			(?SLJIT_SUB_F64 bor ?SLJIT_32)).
-define(SLJIT_MUL_F64,			(?SLJIT_FOP2_BASE + 2)).
-define(SLJIT_MUL_F32,			(?SLJIT_MUL_F64 bor ?SLJIT_32)).
-define(SLJIT_DIV_F64,			(?SLJIT_FOP2_BASE + 3)).
-define(SLJIT_DIV_F32,			(?SLJIT_DIV_F64 bor ?SLJIT_32)).

-define(SLJIT_FOP2R_BASE,		192).
-define(SLJIT_COPYSIGN_F64,		(?SLJIT_FOP2R_BASE + 0)).
-define(SLJIT_COPYSIGN_F32,		(?SLJIT_COPYSIGN_F64 bor ?SLJIT_32)).
-define(SLJIT_COPY_TO_F64,		1).
-define(SLJIT_COPY32_TO_F32,		(?SLJIT_COPY_TO_F64 bor ?SLJIT_32)).
-define(SLJIT_COPY_FROM_F64,		2).
-define(SLJIT_COPY32_FROM_F32,		(?SLJIT_COPY_FROM_F64 bor ?SLJIT_32)).

-define(SLJIT_EQUAL,			0).
-define(SLJIT_ZERO,			?SLJIT_EQUAL).
-define(SLJIT_NOT_EQUAL,		1).
-define(SLJIT_NOT_ZERO,			?SLJIT_NOT_EQUAL).
-define(SLJIT_LESS,			2).
-define(SLJIT_SET_LESS,			?SLJIT_SET(?SLJIT_LESS)).
-define(SLJIT_GREATER_EQUAL,		3).
-define(SLJIT_SET_GREATER_EQUAL,	?SLJIT_SET(?SLJIT_LESS)).  %% BUG?
-define(SLJIT_GREATER,			4).
-define(SLJIT_SET_GREATER,		?SLJIT_SET(?SLJIT_GREATER)).
-define(SLJIT_LESS_EQUAL,		5).
-define(SLJIT_SET_LESS_EQUAL,		?SLJIT_SET(?SLJIT_GREATER)).  %% BUG?
-define(SLJIT_SIG_LESS,			6).
-define(SLJIT_SET_SIG_LESS,		?SLJIT_SET(?SLJIT_SIG_LESS)).
-define(SLJIT_SIG_GREATER_EQUAL,	7).
-define(SLJIT_SET_SIG_GREATER_EQUAL,	?SLJIT_SET(?SLJIT_SIG_LESS)). %% BUG?
-define(SLJIT_SIG_GREATER,		8).
-define(SLJIT_SET_SIG_GREATER,		?SLJIT_SET(?SLJIT_SIG_GREATER)).
-define(SLJIT_SIG_LESS_EQUAL,		9).
-define(SLJIT_SET_SIG_LESS_EQUAL,	?SLJIT_SET(?SLJIT_SIG_GREATER)). %%BUG?
-define(SLJIT_OVERFLOW,			10).
-define(SLJIT_SET_OVERFLOW,		?SLJIT_SET(?SLJIT_OVERFLOW)).
-define(SLJIT_NOT_OVERFLOW,		11).
-define(SLJIT_CARRY,			12).
-define(SLJIT_SET_CARRY,		?SLJIT_SET(?SLJIT_CARRY)).
-define(SLJIT_NOT_CARRY,		13).
-define(SLJIT_ATOMIC_STORED,		14).
-define(SLJIT_SET_ATOMIC_STORED,	?SLJIT_SET(?SLJIT_ATOMIC_STORED)).
-define(SLJIT_ATOMIC_NOT_STORED,	15).
-define(SLJIT_F_EQUAL,			16).
-define(SLJIT_SET_F_EQUAL,		?SLJIT_SET(?SLJIT_F_EQUAL)).
-define(SLJIT_F_NOT_EQUAL,		17).
-define(SLJIT_SET_F_NOT_EQUAL,		?SLJIT_SET(?SLJIT_F_EQUAL)).
-define(SLJIT_F_LESS,			18).
-define(SLJIT_SET_F_LESS,		?SLJIT_SET(?SLJIT_F_LESS)).
-define(SLJIT_F_GREATER_EQUAL,		19).
-define(SLJIT_SET_F_GREATER_EQUAL,	?SLJIT_SET(?SLJIT_F_LESS)).
-define(SLJIT_F_GREATER,		20).
-define(SLJIT_SET_F_GREATER,		?SLJIT_SET(?SLJIT_F_GREATER)).
-define(SLJIT_F_LESS_EQUAL,		21).
-define(SLJIT_SET_F_LESS_EQUAL,		?SLJIT_SET(?SLJIT_F_GREATER)).
-define(SLJIT_UNORDERED,		22).
-define(SLJIT_SET_UNORDERED,		?SLJIT_SET(?SLJIT_UNORDERED)).

-define(SLJIT_ORDERED,			23).
-define(SLJIT_SET_ORDERED,		?SLJIT_SET(?SLJIT_UNORDERED)).
-define(SLJIT_ORDERED_EQUAL,		24).
-define(SLJIT_SET_ORDERED_EQUAL,	?SLJIT_SET(?SLJIT_ORDERED_EQUAL)).
-define(SLJIT_UNORDERED_OR_NOT_EQUAL,	25).
-define(SLJIT_SET_UNORDERED_OR_NOT_EQUAL, ?SLJIT_SET(?SLJIT_ORDERED_EQUAL)).
-define(SLJIT_ORDERED_LESS,		26).
-define(SLJIT_SET_ORDERED_LESS,		?SLJIT_SET(?SLJIT_ORDERED_LESS)).
-define(SLJIT_UNORDERED_OR_GREATER_EQUAL, 27).
-define(SLJIT_SET_UNORDERED_OR_GREATER_EQUAL, ?SLJIT_SET(?SLJIT_ORDERED_LESS)).
-define(SLJIT_ORDERED_GREATER,		28).
-define(SLJIT_SET_ORDERED_GREATER,	?SLJIT_SET(?SLJIT_ORDERED_GREATER)).
-define(SLJIT_UNORDERED_OR_LESS_EQUAL,	29).
-define(SLJIT_SET_UNORDERED_OR_LESS_EQUAL, ?SLJIT_SET(?SLJIT_ORDERED_GREATER)).
-define(SLJIT_UNORDERED_OR_EQUAL,	30).
-define(SLJIT_SET_UNORDERED_OR_EQUAL,	?SLJIT_SET(?SLJIT_UNORDERED_OR_EQUAL)).
-define(SLJIT_ORDERED_NOT_EQUAL,	31).
-define(SLJIT_SET_ORDERED_NOT_EQUAL,	?SLJIT_SET(?SLJIT_UNORDERED_OR_EQUAL)).
-define(SLJIT_UNORDERED_OR_LESS,	32).
-define(SLJIT_SET_UNORDERED_OR_LESS,	?SLJIT_SET(?SLJIT_UNORDERED_OR_LESS)).
-define(SLJIT_ORDERED_GREATER_EQUAL,	33).
-define(SLJIT_SET_ORDERED_GREATER_EQUAL, ?SLJIT_SET(?SLJIT_UNORDERED_OR_LESS)).
-define(SLJIT_UNORDERED_OR_GREATER,	34).
-define(SLJIT_SET_UNORDERED_OR_GREATER,?SLJIT_SET(?SLJIT_UNORDERED_OR_GREATER)).
-define(SLJIT_ORDERED_LESS_EQUAL,	35).
-define(SLJIT_SET_ORDERED_LESS_EQUAL, ?SLJIT_SET(?SLJIT_UNORDERED_OR_GREATER)).

-define(SLJIT_JUMP,		36).
-define(SLJIT_FAST_CALL,	37).
-define(SLJIT_CALL,		38).
-define(SLJIT_CALL_REG_ARG,	39).
-define(SLJIT_REWRITABLE_JUMP,	16#1000).
-define(SLJIT_CALL_RETURN,	16#2000).
-define(SLJIT_MEM_LOAD,		16#000000).
-define(SLJIT_MEM_STORE,	16#000200).
-define(SLJIT_MEM_UNALIGNED,	16#000400).
-define(SLJIT_MEM_ALIGNED_16,	16#000800).
-define(SLJIT_MEM_ALIGNED_32,	16#001000).
-define(SLJIT_MEM_PRE,		16#000000).
-define(SLJIT_MEM_POST,		16#000400).
-define(SLJIT_MEM_SUPP,		16#000800).
-define(SLJIT_SIMD_LOAD,	16#000000).
-define(SLJIT_SIMD_STORE,	16#000001).
-define(SLJIT_SIMD_FLOAT,	16#000400).
-define(SLJIT_SIMD_TEST,	16#000800).

-define(SLJIT_SIMD_REG_64,	(3 bsl 12)).
-define(SLJIT_SIMD_REG_128,	(4 bsl 12)).
-define(SLJIT_SIMD_REG_256,	(5 bsl 12)).
-define(SLJIT_SIMD_REG_512,	(6 bsl 12)).
-define(SLJIT_SIMD_ELEM_8,	(0 bsl 18)).
-define(SLJIT_SIMD_ELEM_16,	(1 bsl 18)).
-define(SLJIT_SIMD_ELEM_32,	(2 bsl 18)).
-define(SLJIT_SIMD_ELEM_64,	(3 bsl 18)).
-define(SLJIT_SIMD_ELEM_128,	(4 bsl 18)).
-define(SLJIT_SIMD_ELEM_256,	(5 bsl 18)).

-define(SLJIT_SIMD_MEM_UNALIGNED,	(0 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_16,	(1 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_32,	(2 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_64,	(3 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_128,	(4 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_256,	(5 bsl 24)).
-define(SLJIT_SIMD_MEM_ALIGNED_512,	(6 bsl 24)).
-define(SLJIT_SIMD_LANE_ZERO,		16#000002).
-define(SLJIT_SIMD_LANE_SIGNED,		16#000004).
-define(SLJIT_SIMD_EXTEND_SIGNED,	16#000002).
-define(SLJIT_SIMD_EXTEND_16,		(1 bsl 24)).
-define(SLJIT_SIMD_EXTEND_32,		(2 bsl 24)).
-define(SLJIT_SIMD_EXTEND_64,		(3 bsl 24)).

-define(SLJIT_SIMD_OP2_AND,	16#000001).
-define(SLJIT_SIMD_OP2_OR,	16#000002).
-define(SLJIT_SIMD_OP2_XOR,	16#000003).
-define(SLJIT_SIMD_OP2_SHUFFLE,	16#000004).
-define(SLJIT_ATOMIC_TEST,      16#10000).
-define(SLJIT_ATOMIC_USE_CAS,   16#20000).
-define(SLJIT_ATOMIC_USE_LS,    16#40000).
-define(SLJIT_GP_REGISTER,      0).
-define(SLJIT_FLOAT_REGISTER,   1).
-define(SLJIT_CURRENT_FLAGS_32,	      ?SLJIT_32).
-define(SLJIT_CURRENT_FLAGS_ADD,      16#01).
-define(SLJIT_CURRENT_FLAGS_SUB,      16#02).
-define(SLJIT_CURRENT_FLAGS_COMPARE,  16#04).
-define(SLJIT_SERIALIZE_IGNORE_DEBUG, 16#1).


-define(OP0_LIST,
	?OP0_NAME(breakpoint, ?SLJIT_BREAKPOINT)
	?OP0_NAME(nop, ?SLJIT_NOP)
	?OP0_NAME(lmul_uw, ?SLJIT_LMUL_UW)
	?OP0_NAME(lmul_sw, ?SLJIT_LMUL_SW)
	?OP0_NAME(divmod_uw, ?SLJIT_DIVMOD_UW)
	?OP0_NAME(divmod_u32, ?SLJIT_DIVMOD_U32)
	?OP0_NAME(divmod_sw, ?SLJIT_DIVMOD_SW)
	?OP0_NAME(divmod_s32, ?SLJIT_DIVMOD_S32)
	?OP0_NAME(div_uw, ?SLJIT_DIVMOD_UW)
	?OP0_NAME(div_u32, ?SLJIT_DIVMOD_U32)
	?OP0_NAME(div_sw, ?SLJIT_DIVMOD_SW)
	?OP0_NAME(div_s32, ?SLJIT_DIVMOD_S32)
	?OP0_NAME(memory_barrier, ?SLJIT_MEMORY_BARRIER)
	?OP0_NAME(endbr, ?SLJIT_ENDBR)
	?OP0_NAME(skip_frames_before_return, ?SLJIT_SKIP_FRAMES_BEFORE_RETURN)
       ).

-define(OP1_LIST,
	?OP1_NAME(mov, ?SLJIT_MOV)
	?OP1_NAME(mov_u8, ?SLJIT_MOV_U8)
	?OP1_NAME(mov32_u8, ?SLJIT_MOV32_U8)
	?OP1_NAME(mov_s8, ?SLJIT_MOV_S8)
	?OP1_NAME(mov32_s8, ?SLJIT_MOV32_S8)
	?OP1_NAME(mov_u16, ?SLJIT_MOV_U16)
	?OP1_NAME(mov32_u16, ?SLJIT_MOV32_U16)
	?OP1_NAME(mov_s16, ?SLJIT_MOV_S16)
	?OP1_NAME(mov32_s16, ?SLJIT_MOV32_S16)
	?OP1_NAME(mov_u32, ?SLJIT_MOV_U32)
	?OP1_NAME(mov32_u32, ?SLJIT_MOV32)
	?OP1_NAME(mov_s32, ?SLJIT_MOV_S32)
	?OP1_NAME(mov32_s32, ?SLJIT_MOV32)
	?OP1_NAME(mov32, ?SLJIT_MOV32)
	?OP1_NAME(mov_p, ?SLJIT_MOV_P)
	?OP1_NAME(clz, ?SLJIT_CLZ)
	?OP1_NAME(clz32, ?SLJIT_CLZ32)
	?OP1_NAME(ctz, ?SLJIT_CTZ)
	?OP1_NAME(ctz32, ?SLJIT_CTZ32)
	?OP1_NAME(rev, ?SLJIT_REV)
	?OP1_NAME(rev32, ?SLJIT_REV32)
	?OP1_NAME(rev_u16, ?SLJIT_REV_U16)
	?OP1_NAME(rev32_u16, ?SLJIT_REV32_U16)
	?OP1_NAME(rev_s16, ?SLJIT_REV_S16)
	?OP1_NAME(rev32_s16, ?SLJIT_REV32_S16)
	?OP1_NAME(rev_u32, ?SLJIT_REV_U32)
	?OP1_NAME(rev_s32, ?SLJIT_REV_S32)).

-define(OP2_LIST,
	?OP2_NAME(add, ?SLJIT_ADD)
	?OP2_NAME(add32, ?SLJIT_ADD32)
	?OP2_NAME(addc, ?SLJIT_ADDC)
	?OP2_NAME(addc32, ?SLJIT_ADDC32)
	?OP2_NAME(sub, ?SLJIT_SUB)
	?OP2_NAME(sub32, ?SLJIT_SUB32)
	?OP2_NAME(subc, ?SLJIT_SUBC)
	?OP2_NAME(subc32, ?SLJIT_SUB32)
	?OP2_NAME(mul, ?SLJIT_MUL)
	?OP2_NAME(mul32, ?SLJIT_MUL32)
	?OP2_NAME('and', ?SLJIT_AND)
	?OP2_NAME(and32, ?SLJIT_AND32)
	?OP2_NAME('or', ?SLJIT_OR)
	?OP2_NAME(or32, ?SLJIT_OR32)
	?OP2_NAME('xor', ?SLJIT_XOR)
	?OP2_NAME(xor32, ?SLJIT_XOR32)
	?OP2_NAME(shl, ?SLJIT_SHL)
	?OP2_NAME(shl32, ?SLJIT_SHL32)
	?OP2_NAME(mshl, ?SLJIT_MSHL)
	?OP2_NAME(mshl32, ?SLJIT_MSHL32)
	?OP2_NAME(lshr, ?SLJIT_LSHR)
	?OP2_NAME(lshr32, ?SLJIT_LSHR32)
	?OP2_NAME(mlshr, ?SLJIT_MLSHR)
	?OP2_NAME(mlshr32, ?SLJIT_MLSHR32)
	?OP2_NAME(ashr, ?SLJIT_ASHR)
	?OP2_NAME(ashr32, ?SLJIT_ASHR32)
	?OP2_NAME(mashr, ?SLJIT_MASHR)
	?OP2_NAME(mashr32, ?SLJIT_MASHR32)
	?OP2_NAME(rotl, ?SLJIT_ROTL)
	?OP2_NAME(rotl32, ?SLJIT_ROTL32)
	?OP2_NAME(rotr, ?SLJIT_ROTR)
	?OP2_NAME(rotr32, ?SLJIT_ROTR32)).

-define(OP_SI_LIST,
	?OP_SI_NAME(shl, ?SLJIT_SHL)
	?OP_SI_NAME(shl32, ?SLJIT_SHL32)
	?OP_SI_NAME(mshl, ?SLJIT_MSHL)
	?OP_SI_NAME(mshl32, ?SLJIT_MSHL32)
	?OP_SI_NAME(lshr, ?SLJIT_LSHR)
	?OP_SI_NAME(lshr32, ?SLJIT_LSHR32)
	?OP_SI_NAME(mlshr, ?SLJIT_MLSHR)
	?OP_SI_NAME(mlshr32, ?SLJIT_MLSHR32)
       ).

-define(OP_SRC_LIST,
	?OP_SRC_NAME(fast_return, ?SLJIT_FAST_RETURN)
	?OP_SRC_NAME(skip_frames_before_fast_return, ?SLJIT_SKIP_FRAMES_BEFORE_FAST_RETURN)
	?OP_SRC_NAME(prefetch_l1, ?SLJIT_PREFETCH_L1)
	?OP_SRC_NAME(prefetch_l2, ?SLJIT_PREFETCH_L2)
	?OP_SRC_NAME(prefetch_l3, ?SLJIT_PREFETCH_L3)
	?OP_SRC_NAME(prefetch_once, ?SLJIT_PREFETCH_ONCE)).

-define(OP_SRC_DST_LIST,
	?OP_DSR_NAME(fast_enter, ?SLJIT_FAST_ENTER)
	?OP_DSR_NAME(get_return_address, ?SLJIT_GET_RETURN_ADDRESS)).

-define(FOP1_LIST,
	?FOP1_NAME(mov_f64, ?SLJIT_MOV_F64)
	?FOP1_NAME(mov_f32, ?SLJIT_MOV_F32)
	?FOP1_NAME(conv_f64_from_f32, ?SLJIT_CONV_F64_FROM_F32)
	?FOP1_NAME(conv_f32_from_f64, ?SLJIT_CONV_F32_FROM_F64)
	?FOP1_NAME(conv_sw_from_f64, ?SLJIT_CONV_SW_FROM_F64)
	?FOP1_NAME(conv_sw_from_f32, ?SLJIT_CONV_SW_FROM_F32)
	?FOP1_NAME(conv_s32_from_f64, ?SLJIT_CONV_S32_FROM_F64)
	?FOP1_NAME(conv_s32_from_f32, ?SLJIT_CONV_S32_FROM_F32)
	?FOP1_NAME(conv_f64_from_sw, ?SLJIT_CONV_F64_FROM_SW)
	?FOP1_NAME(conv_f32_from_sw, ?SLJIT_CONV_F32_FROM_SW)
	?FOP1_NAME(conv_f64_from_s32, ?SLJIT_CONV_F64_FROM_S32)
	?FOP1_NAME(conv_f32_from_s32, ?SLJIT_CONV_F32_FROM_S32)
	?FOP1_NAME(conv_f64_from_uw, ?SLJIT_CONV_F64_FROM_UW)
	?FOP1_NAME(conv_f32_from_uw, ?SLJIT_CONV_F32_FROM_UW)
	?FOP1_NAME(conv_f64_from_u32, ?SLJIT_CONV_F64_FROM_U32)
	?FOP1_NAME(conv_f32_from_u32, ?SLJIT_CONV_F32_FROM_U32)
	?FOP1_NAME(cmp_f64, ?SLJIT_CMP_F64)
	?FOP1_NAME(cmp_f32, ?SLJIT_CMP_F32)
	?FOP1_NAME(neg_f64, ?SLJIT_NEG_F64)
	?FOP1_NAME(neg_f32, ?SLJIT_NEG_F32)
	?FOP1_NAME(abs_f64, ?SLJIT_ABS_F64)
	?FOP1_NAME(abs_f32, ?SLJIT_ABS_F32)).

-define(FOP2_LIST,
	?FOP2_NAME(add_f64, ?SLJIT_ADD_F64)
	?FOP2_NAME(add_f32, ?SLJIT_ADD_F32)
	?FOP2_NAME(sub_f64, ?SLJIT_SUB_F64)
	?FOP2_NAME(sub_f32, ?SLJIT_SUB_F32)
	?FOP2_NAME(mul_f64, ?SLJIT_MUL_F64)
	?FOP2_NAME(mul_f32, ?SLJIT_MUL_F32)
	?FOP2_NAME(div_f64, ?SLJIT_DIV_F64)
	?FOP2_NAME(div_f32, ?SLJIT_DIV_F32)).

-define(OP_FCOPY_LIST,
	?OP_FCOPY_NAME(copy_to_f64, ?SLJIT_COPY_TO_F64)
	?OP_FCOPY_NAME(copy32_to_f32, ?SLJIT_COPY32_TO_F32)
	?OP_FCOPY_NAME(copy_from_f64, ?SLJIT_COPY_FROM_F64)
	?OP_FCOPY_NAME(copy32_from_f32, ?SLJIT_COPY32_FROM_F32)).

-define(SIMD_OP2_LIST,
	?SIMD_OP2_NAME(vand, ?SLJIT_SIMD_OP2_AND)
	?SIMD_OP2_NAME(vor, ?SLJIT_SIMD_OP2_OR)
	?SIMD_OP2_NAME(vxor, ?SLJIT_SIMD_OP2_XOR)
	?SIMD_OP2_NAME(shuffle, ?SLJIT_SIMD_OP2_SHUFFLE)).

-endif.

