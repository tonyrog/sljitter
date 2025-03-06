%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    SLJIT assembler header file
%%% @end
%%% Created : 23 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-ifndef(SLJIT_ASM_HRL).
-define(SLJIT_ASM_HRL, true).

%% instruction formats (so far)
-define(FMT_OP0,    1).
-define(FMT_OP1,    2).
-define(FMT_OP2,    3).
-define(FMT_OP2U,   4).
-define(FMT_OP2R,   5).
-define(FMT_SI,     6).
-define(FMT_OP_SRC, 7).
-define(FMT_OP_DST, 8).
-define(FMT_FOP1,   9).
-define(FMT_FOP2,   10).
-define(FMT_FOP2R,  11).
-define(FMT_FSET32, 12).
-define(FMT_FSET64, 13).
-define(FMT_FCOPY,  14).
-define(FMT_LABEL,  15).
-define(FMT_JUMP,   16).
-define(FMT_CALL,   17).
-define(FMT_CMP,    18).
-define(FMT_FCMP,   19).
-define(FMT_IJUMP,       20).
-define(FMT_ICALL,       21).
-define(FMT_ENTER,       22).
-define(FMT_SET_CONTEXT, 23).
-define(FMT_RETURN,      24).
-define(FMT_RETURN_VOID, 25).
-define(FMT_SIMD_OP2,    26).
-define(FMT_MCALL,       27).  %% module:function call
-define(FMT_MJUMP,       28).  %% module:name jump
-define(FMT_CONST,       29).
-define(FMT_MOV_ADDR,    30).

%% synthetic formats label names constants etc
-define(FMT_LABEL_NAME,  16#80+1).  %% unsigned integer
-define(FMT_MODULE,      16#80+2).  %% string (=> atom)
-define(FMT_FUNCTION,    16#80+3).  %% string (=> atom)
-define(FMT_CONST_NAME,  16#80+4).
-define(FMT_JUMP_NAME,   16#80+5).  %% strint (=> atom)
%% -define(FMT_LAST, 	 28).

%% instruction layout
%%   u8   u8
%% +----+-----+------+----------------+
%% | Sz | fmt | arg1 | arg2 ...  argN |
%% +----+-----+------+----------------+
%%
%% Each argument is a dyamically sized field
%% 
%% first bytes is most significant
%% 
%% +----+----+------------+
%% |Cont|Sign| 6 bit value|
%% +----+----+------------+
%%
%% Where Cont is 1 if more bytes follow
%% Sign is 1 if value is negative
%%
%% Followed by continuation bytes
%% +----+------------+
%% |Cont| 7 bit value|
%% +----+------------+
%%

-define(FMT_LIST,
	?FMT_NAME(op0, ?FMT_OP0)
	?FMT_NAME(op1, ?FMT_OP1)
	?FMT_NAME(op2, ?FMT_OP2)
	?FMT_NAME(op2u, ?FMT_OP2U)
	?FMT_NAME(op2r, ?FMT_OP2R)
	?FMT_NAME(shift_into, ?FMT_SI)
	?FMT_NAME(op_src, ?FMT_OP_SRC)
	?FMT_NAME(op_dst, ?FMT_OP_DST)
	?FMT_NAME(fop1, ?FMT_FOP1)
	?FMT_NAME(fop2, ?FMT_FOP2)
	?FMT_NAME(fop2r, ?FMT_FOP2R)
	?FMT_NAME(fset32, ?FMT_FSET32)
	?FMT_NAME(fset64, ?FMT_FSET64)
	?FMT_NAME(fcopy, ?FMT_FCOPY)
	?FMT_NAME(label, ?FMT_LABEL)
	?FMT_NAME(const, ?FMT_CONST)
	?FMT_NAME(jump, ?FMT_JUMP)
	?FMT_NAME(call, ?FMT_CALL)
	?FMT_NAME(cmp, ?FMT_CMP)
	?FMT_NAME(fcmp, ?FMT_FCMP)
	?FMT_NAME(ijump, ?FMT_IJUMP)
	?FMT_NAME(icall, ?FMT_ICALL)
	?FMT_NAME(mcall, ?FMT_MCALL)
	?FMT_NAME(enter, ?FMT_ENTER)
	?FMT_NAME(set_context, ?FMT_SET_CONTEXT)
	?FMT_NAME(return_void, ?FMT_RETURN_VOID)
	?FMT_NAME(return, ?FMT_RETURN)
	?FMT_NAME(simd_op2, ?FMT_SIMD_OP2)
	?FMT_NAME(label_name, ?FMT_LABEL_NAME)
	?FMT_NAME(const_name, ?FMT_CONST_NAME)
	?FMT_NAME(jump_name, ?FMT_JUMP_NAME)
	?FMT_NAME(module, ?FMT_MODULE)
	?FMT_NAME(function, ?FMT_FUNCTION)
	?FMT_NAME(mov_addr, ?FMT_MOV_ADDR)
       ).

-endif.
