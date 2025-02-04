%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%     Assembler for symbolic SLJIT instructions
%%% @end
%%% Created : 19 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit_asm).

-export([assemble/1, assemble/2]).
-export([load/1]).
-export([save_as_bin/2]).
-export([asm_ins_list/3, asm_ins/3]).
-export([slo_ins_list/3, slo_ins/3]).

%% debug
-export([encode_fmt_arg/1, decode_fmt_arg/1]).
-export([encode_fmt_args/2, decode_fmt_args/2]).

-include("../include/sljit.hrl").
-include("../include/sljit_asm.hrl").

-define(dbg(Fmt, Args), io:format(Fmt, Args)).
%%
%% operation variants = .32 .nz
%% .u8, .u16, .u32, uw, .s8, .s16, .s32, sw
%% 
-type op0() :: breakpoint | nop | lmul_uw | lmul_sw | divmod_uw |
	       divmod_u32 | divmod_sw | divmod_s32 | div_uw |
	       div_u32 | div_sw | div_s32 | memory_barrier |
	       endbr | skip_frames_before_return.

-type op1() :: mov | mov_u8 | mov32_u8 | mov_s8 | mov32_s8 | 
	       mov_u16 | mov32_u16 | mov_s16 | mov32_s16 |
	       mov_u32 | mov32_u32 | mov_s32 | mov32_s32 |
	       mov32 | mov_p | clz | clz32 | ctz | ctz32 |
	       rev | rev32 | rev_u16 | rev32_u16 | rev_s16 |
	       rev32_s16 | rev_u32 | rev_s32.

-type op2() :: add | add32 | addc | addc32 | sub | sub32 | subc | subc32 |
	       mul | mul32 | 'and' | and32 | 'or' | or32 | 'xor' | xor32 |
	       shl | shl32 |
	       mshl | mshl32 | lshr | lshr32 | mlshr | mlshr32 | ashr | 
	       ashr32 | mashr | mashr32 | rotl | rotl32 | rotr | rotr32.

-type op2r() :: muladd | muladd32.

-type op_shift_into() ::
	shl | shl32 | mshl | mshl32 | lshr | lshr32 |
	mlshr | mlshr32 | lshr | lshr32 |
	shl_nz | shl32_nz | mshl_nz | mshl32_nz | lshr_nz | lshr32_nz |
	mlshr_nz | mlshr32_nz | lshr_nz | lshr32_nz.

-type op_src() :: fast_return | skip_frames_before_fast_return |
		  prefetch_l1 | prefetch_l2 | prefetch_l3 | prefetch_once.

-type op_dst() :: fast_enter | get_return_address.

%% suffix .f64 .f32
-type fop1() :: mov_f64 | mov_f32 | conv_f64_from_f32 | conv_f32_from_f64 |
		conv_sw_from_f64 | conv_sw_from_f32 | conv_s32_from_f64 |
		conv_s32_from_f32 | conv_f64_from_sw | conv_f32_from_sw |
		conv_f64_from_s32 | conv_f32_from_s32 | conv_f64_from_uw |
		conv_f32_from_uw | conv_f64_from_u32 | conv_f32_from_u32 |
		cmp_f64 | cmp_f32 | neg_f64 | neg_f32 | abs_f64 | abs_f32.

-type fop2() :: add_f64 | add_f32 | sub_f64 | sub_f32 | mul_f64 | mul_f32 |
		div_f64 | div_f32.

-type fop2r() :: copysign_f64 | copysign_f32.

-type op_fcopy() :: copy_to_f64 | copy32_to_f32 | copy_from_f64 | 
		    copy32_from_f32.

-type simd_op2() :: vand | vor | vxor | shuffle.

-type scratch_reg() :: r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7 | r8 | r9 |
		       {r, 0..255}.
-type saved_reg() :: s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | s8 | s9 |
		     {s, 0..255}.
-type reg() :: scratch_reg() | saved_reg().

-type float_reg() :: 
	fr0 | fr1 | fr2 | fr3 | fr4 | fr5 | fr6 | fr7 | fr8 | fr9 |
	{fr, 0..255}.
-type float_saved_reg() ::
	fs0 | fs1 | fs2 | fs3 | fs4 | fs5 | fs6 | fs7 | fs8 | fs9 |
	{fs, 0..255}.
-type freg() :: float_reg() | float_saved_reg().

-type vec_reg() :: vr0 | vr1 | vr2 | vr3 | vr4 | vr5 | vr6 | vr7 | vr8 | vr9 |
		   {vr, 0..255}.
-type vec_saved_reg() ::
	vs0 | vs1 | vs2 | vs3 | vs4 | vs5 | vs6 | vs7 | vs8 | vs9 | 
	{vs, 0..255}.
-type vreg() :: vec_reg() | vec_saved_reg().

-type mem() :: {mem, integer()} |              %% [imm]
	       {mem, reg()} |                  %% [reg]
	       {mem, reg(), integer()} |       %% [reg + imm]
	       {mem, reg(), reg()} |           %% [reg + reg]
	       {mem, reg(), reg(), integer()}. %% [reg + (reg<<imm)]

-type imm() :: {imm, integer() | float()}.

%% Assemble a file.asm into slo object binary
-spec assemble(Filename::string()) -> {ok, binary()} | {error, term()}.
assemble(Filename) ->
    case file:consult(Filename) of
	{ok, Instructions} ->
	    case file:open(<<>>, [ram, binary, write]) of
		{ok, Fd} ->
		    Compile = sljit:create_compiler(),
		    _Sym = asm_ins_list({fd,Fd,Compile}, Instructions, #{}),
		    {ok, ObjBin} = ram_file:get_file(Fd),
		    file:close(Fd),
		    {ok, ObjBin};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

-spec assemble(Filename::string(), DstFilename::string()) -> 
	  ok | {error, term()}.
assemble(Filename, DstFilename) ->
    Ext = filename:extension(DstFilename),
    DstFilename1 = case Ext of
		       ".slo" -> DstFilename;
		       ".bin" -> DstFilename;
		       _ -> DstFilename ++ ".slo"
		   end,
    case file:consult(Filename) of
	{ok, Instructions} ->
	    WriteOpts = if Ext =:= ".bin" -> [ram, binary, write];
			   true -> [write]
			end,
	    case file:open(DstFilename1, WriteOpts) of
		{ok, Fd} ->
		    Compile = sljit:create_compiler(),
		    _Sym = asm_ins_list({fd,Fd,Compile}, Instructions, #{}),
		    if Ext =:= ".bin" ->
			    {_,Code} = sljit:generate_code(Compile),
			    Bin = sljit:get_code(Code),
			    file:close(Fd),
			    file:write_file(DstFilename1, Bin);
		       true ->
			    file:close(Fd)
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

save_as_bin(Code, DstFilename) ->
    DstFilename1 = case filename:extension(DstFilename) of
		       ".bin" -> DstFilename;
		       _ -> DstFilename ++ ".bin"
		   end,    
    Bin = sljit:get_code(Code),
    file:write_file(DstFilename1, Bin).

load(Filename) ->
    case filename:extension(Filename) of
	".asm" ->
	    case file:consult(Filename) of
		{ok, InsList} ->
		    Compile = sljit:create_compiler(),
		    St = asm_ins_list(Compile, InsList, #{}),
		    sljit:generate_code(Compile);
		Error ->
		    Error
	    end;
	".slo" ->
	    case load_slo(Filename) of
		{ok, Slo} ->
		    Compile = sljit:create_compiler(),
		    St = slo_ins_list(Compile, Slo, #{}),
		    sljit:generate_code(Compile);
		Error ->
		    Error
	    end;
	_ ->
	    {error, {unknown_file_type, Filename}}
    end.

load_slo(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Compile = sljit:create_compiler(),
	    load_object(Compile, Bin, []);
	Error ->
	    Error
    end.

load_object(_, <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
load_object(Compile, <<Sz, Data:Sz/binary, Rest/binary>>, Acc) ->
    <<F, Args/binary>> = Data,
    Ins = load_ins(F, Args),
    load_object(Compile, Rest, [Ins|Acc]).

load_ins(F, Bin) ->
    Sig = fmt_signature(F),
    As = decode_fmt_args(Sig, Bin),
    {F, As}.

slo_ins_list(Compile, [Ins|InsList], Sym) ->
    Sym1 = slo_ins(Compile, Ins, Sym),
    slo_ins_list(Compile, InsList, Sym1);
slo_ins_list(_, [], Sym) ->
    Sym.
    
slo_ins(Compile, Ins, St) ->
    io:format("slo ~w\n", [Ins]),
    case Ins of
	{?FMT_MODULE, [M]} ->  %% synthetic
	    Mod = list_to_atom(M),
	    ok = sljit:module(Compile, Mod),
	    St#{module => Mod};
	{?FMT_FUNCTION, [F]} ->  %% synthetic
	    Fun = list_to_atom(F),
	    ok = sljit:function(Compile, Fun),
	    St#{function => Fun};
	{?FMT_LABEL_NAME, [L]} ->  %% synthetic
	    St#{label_name => L};
	{?FMT_OP0,[Op]} -> 
	    ok = sljit:emit_op0(Compile, Op),
	    St;
	{?FMT_OP1, [Op, Dst, Dstw, Src, Srcw]} ->
	    ok = sljit:emit_op1(Compile, Op, Dst, Dstw, Src, Srcw),
	    St;
	{?FMT_OP2, [Op, Dst, Dstw, Src1, Src1w, Src2, Src2w]} ->
	    ok =sljit:emit_op2(Compile, Op, Dst, Dstw, Src1, Src1w, 
			       Src2, Src2w),
	    St;
	{?FMT_OP2U, [Op, Src1, Src1w, Src2, Src2w]} ->
	    ok = sljit:emit_op2u(Compile, Op, Src1,Src1w, Src2,Src2w),
	    St;
	{?FMT_OP2R, [Op, DR, Src1, Src1w, Src2, Src2w]} ->
	    ok = sljit:emit_op2r(Compile, Op, DR, Src1,Src1w, Src2,Src2w),
	    St;
	{?FMT_SI, [Op, Dst, Src1, Src2, Src3, Src3w]} ->
	    ok = sljit:emit_shift_into(Compile, Op, Dst, Src1,Src2, Src3,Src3w),
	    St;       
	%% ?FMT_OP_src -> ins_op_src(Compile, Instruction, St);
	%% ?FMT_OP_dst -> ins_op_dst(Compile, Instruction, St);
	{?FMT_FOP1, [Op, Dst, Dstw, Src, Srcw]} ->
	    ok = sljit:emit_fop1(Compile, Op, Dst, Dstw, Src, Srcw),
	    St;
	{?FMT_FOP2, [Op, Dst, Dstw, Src1, Src1w, Src2, Src2w]} ->
	    ok = sljit:emit_fop2(Compile, Op, Dst,Dstw, Src1,Src1w, Src2,Src2w),
	    St;
	{?FMT_FOP2R, [Op, DR, Src1, Src1w, Src2, Src2w]} ->
	    ok = sljit:emit_fop2r(Compile, Op, DR, Src1, Src1w, Src2, Src2w),
	    St;
	{?FMT_FSET32, [FReg, Value]} ->
	    ok = sljit:emit_fset32(Compile, FReg, Value),
	    St;
	{?FMT_FSET64, [FReg, Value]} ->
	    ok = sljit:emit_fset64(Compile, FReg, Value),
	    St;
	{?FMT_FCOPY, [Op,FReg,Reg]} ->
	    ok = sljit:emit_fopy(Compile, Op, FReg, Reg),
	    St;
	{?FMT_LABEL, []} -> 
	    L = maps:get(label_name, St),
	    ins_label(Compile, {label, L}, St);

	{?FMT_CMP, [Type, Src1, Src1w, Src2, Src2w]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_cmp(Compile, Type, Src1, Src1w, Src2, Src2w),
	    add_target(L, Jump, St);

	{?FMT_FCMP, [Type, Src1, Src1w, Src2, Src2w]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_fcmp(Compile, Type, Src1, Src1w, Src2, Src2w),
	    add_target(L, Jump, St);
	{?FMT_JUMP, [Type]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_jump(Compile, Type),
	    add_target(L, Jump, St);
	{?FMT_IJUMP,[Type, Src, Srcw]} -> 
	    ok = sljit:emit_ijump(Compile, Type, Src, Srcw),
	    St;
	{?FMT_MJUMP,[Type, Mod, Fun]} -> 
	    ok = sljit:emit_mjump(Compile, Type,
				  list_to_atom(Mod), list_to_atom(Fun)),
	    St;
	{?FMT_CALL, [Type, ArgTypes]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_call(Compile, Type, ArgTypes),
	    add_target(L, Jump, St);
	{?FMT_ICALL, [Type,ArgTypes,Src,Srcw]} ->
	    ok = sljit:emit_icall(Compile, Type, ArgTypes, Src, Srcw),
	    St;
	%% like ICALL but with mod:fun instead
	{?FMT_MCALL, [Type,ArgTypes,Mod,Fun]} ->
	    ok = sljit:emit_mcall(Compile, Type, ArgTypes, 
				  list_to_atom(Mod), list_to_atom(Fun)),
	    St;

	{?FMT_ENTER,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    ok = sljit:emit_enter(Compile,Options,ArgTypes,Scratches,Saved,
				  LocalSize),
	    St;
	{?FMT_SET_CONTEXT,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    ok = sljit:set_context(Compile,Options,ArgTypes,Scratches,Saved,
				   LocalSize),
	    St;
	{?FMT_RETURN_VOID,[]} ->
	    ok = sljit:emit_return_void(Compile),
	    St;
	{?FMT_RETURN,[Op,Src,Srcw]} ->
	    ok = sljit:emit_return(Compile, Op, Src, Srcw),
	    St;
	{?FMT_SIMD_OP2,[Op, DstVReg, Src1VReg, Src2, Src2w]} ->
	    ok = sljit:emit_simd_op2(Compile, Op,DstVReg,
				     Src1VReg, Src2, Src2w),
	    St;
	{Fmt,_Args} -> throw({error, {unknown_format, Fmt}})
    end.

asm_ins_list(Compile, [Ins|Instructions], St) ->
    St1 = asm_ins(Compile, Ins, St),
    asm_ins_list(Compile, Instructions, St1);
asm_ins_list(_, [], St) ->
    St.

asm_ins(Compile, Instruction, St) ->
    Fmt = fmt(Instruction),
    io:format("asm (~w) ~p~n", [Fmt, Instruction]),
    case Fmt of
	?FMT_MODULE ->
	    {module, M} = Instruction,
	    ok = emit(Compile, module, [M]),
	    St#{module => M};
	?FMT_FUNCTION ->
	    {function, F} = Instruction,
	    ok = emit(Compile, function, [F]),
	    St#{function => F};
	?FMT_OP0 -> ins_op0(Compile, Instruction, St);
	?FMT_OP1 -> ins_op1(Compile, Instruction, St);
	?FMT_OP2 -> ins_op2(Compile, Instruction, St);
	%% ?FMT_OP2U -> ins_op2u(Compile, Instruction, St);
	%% ?FMT_OP2R -> ins_op2r(Compile, Instruction, St);
	%% ?FMT_SI -> ins_si(Compile, Instruction, St);
	%% ?FMT_OP_src -> ins_op_src(Compile, Instruction, St);
	%% ?FMT_OP_dst -> ins_op_dst(Compile, Instruction, St);
	?FMT_FOP1 -> ins_fop1(Compile, Instruction, St);
	?FMT_FOP2 -> ins_fop2(Compile, Instruction, St);
	%%?FMT_FOP2R -> ins_fop2r(Compile, Instruction, St);
	?FMT_FSET32 -> ins_fset32(Compile, Instruction, St);
	?FMT_FSET64 -> ins_fset64(Compile, Instruction, St);
	%%?FMT_FCOPY -> ins_fcopy(Compile, Instruction, St);
	?FMT_LABEL -> ins_label(Compile, Instruction, St);
	?FMT_JUMP -> ins_jump(Compile, Instruction, St);
	?FMT_CALL -> ins_call(Compile, Instruction, St);
	%% ?FMT_CMP -> ins_cmp(Compile, Instruction, St);
	%% ?FMT_FCMP -> ins_fcmp(Compile, Instruction, St);
	?FMT_IJUMP -> ins_ijump(Compile, Instruction, St);
	?FMT_ICALL -> ins_icall(Compile, Instruction, St);
	?FMT_ENTER -> ins_enter(Compile, Instruction, St);
	?FMT_SET_CONTEXT -> ins_enter(Compile, Instruction, St);
	?FMT_RETURN -> ins_return(Compile, Instruction, St);
	?FMT_SIMD_OP2 -> ins_simd_op2(Compile, Instruction, St);
	Fmt -> throw({error, {unknown_format, Fmt}})
    end.

-undef(OP0_NAME).
-define(OP0_NAME(I, N), I -> N; ).

ins_op0(Compile, {Ins}, St) ->
    ins_op0(Compile, Ins, St);
ins_op0(Compile, Ins, St) ->
    Op = case Ins of
	     ?OP0_LIST
	     _ -> throw({error, {unknown_op0, Ins}})
	 end,
    ok = emit(Compile, op0, [Op]),
    St.

-undef(OP1_NAME).
-define(OP1_NAME(I, N), I -> N; ).

ins_op1(Compile, {Ins, D, S}, St) ->
    Op = case Ins of
	     ?OP1_LIST
	     _ -> throw({error, {unknown_op1, Ins}})
	 end,
    {Dst, Dstw} = dst(D),
    {Src, Srcw} = src(S),
    ok = emit(Compile, op1, [Op, Dst, Dstw, Src, Srcw]),
    St.

-undef(OP2_NAME).
-define(OP2_NAME(I, N), I -> N; ).

-undef(OP_SI_NAME).
-define(OP_SI_NAME(I, N), I -> (N); ).

ins_op2(Compile, {Ins, S1, S2}, St) ->
    ins_op2(Compile, {Ins, [], S1, S2}, St);
ins_op2(Compile, {Ins, Fs, S1, S2}, St) when is_list(Fs) ->
    Op = case Ins of
	     ?OP2_LIST
	     _ -> throw({error, {unknown_op2, Ins}})
	 end,
    OpF = ins_set_flags(Fs),
    {Src1, Src1w} = src(S1),
    {Src2, Src2w} = src(S2),
    ?dbg("op2u ~w ~w ~w ~w ~w ~w\n", [Op, OpF, Src1, Src1w, Src2, Src2w]),
    ok = emit(Compile, op2u, [Op bor OpF, Src1, Src1w, Src2, Src2w]),
    St;
ins_op2(Compile, {Ins, Fs, D, S1, S2}, St) when is_list(Fs) ->
    Op = case Ins of
	     ?OP2_LIST
	     _ -> throw({error, {unknown_op2, Ins}})
	 end,
    OpF = ins_set_flags(Fs),
    {Src1, Src1w} = src(S1),
    {Src2, Src2w} = src(S2),
    case dst(D) of
	{DR, 0} when ?SLJIT_IS_REG(DR) ->
	    ?dbg("op2r ~w ~w ~w ~w ~w ~w\n",
		 [Op bor OpF, DR, Src1, Src1w, Src2, Src2w]),
	    ok = emit(Compile, op2r,
		      [Op bor OpF, DR, Src1, Src1w, Src2, Src2w]);
	{Dst, Dstw} ->
	    ?dbg("op2 op=~w dst=~w dst2=~w src1=~w src1w=~w src2=~w src2w=~w\n",
		 [Op bor OpF, Dst, Dstw, Src1, Src1w, Src2, Src2w]),
	    ok = emit(Compile, op2,
		      [Op bor OpF, Dst, Dstw, Src1, Src1w, Src2, Src2w])
    end,
    St;
ins_op2(Compile, {Ins, D, S1, S2}, St) ->
    ins_op2(Compile, {Ins, [], D, S1, S2}, St);
ins_op2(Compile, {Ins, D, S1, S2, S3}, St) ->
    ins_op2(Compile, {Ins, [], D, S1, S2, S3}, St);
ins_op2(Compile, {Ins, Fs, D, S1, S2, S3}, St) ->
    Op = case Ins of
	     ?OP_SI_LIST
	     _ -> throw({error, {unknown_shift_into_op, Ins}})
	 end,
    Op1 = case Fs of
	      [] -> Op;
	      [nz] ->  Op bor ?SLJIT_SHIFT_INTO_NON_ZERO
	  end,
    Dst = reg(D),
    Src1 = reg(S1),
    Src2 = reg(S2),
    {Src3,Src3w} = src(S3),
    ?dbg("op_si ~w ~w ~w ~w ~w ~w\n",
	 [Op1, Dst, Src1, Src2, Src3, Src3w]),
    ok = emit(Compile, shift_into, [Op1, Dst, Src1, Src2, Src3, Src3w]),
    St.

-undef(FOP1_NAME).
-define(FOP1_NAME(I, N), I -> N; ).

ins_fop1(Compile, {Ins, D, S}, St) ->
    Op = case Ins of
	     ?FOP1_LIST
	     _ -> throw({error, {unknown_fop1, Ins}})
	 end,
    {Dst, Dstw} = fdst(D),
    {Src, Srcw} = fsrc(S),
    %% fixme: emit imm float ?
    ok = emit(Compile, fop1, [Op, Dst, Dstw, Src, Srcw]),
    St.

-undef(FOP2_NAME).
-define(FOP2_NAME(I, N), I -> N; ).

ins_fop2(Compile, {Ins, D, S1, S2}, St) ->
    ins_fop2(Compile, {Ins, [], D, S1, S2}, St);
ins_fop2(Compile, {Ins, Fs, D, S1, S2}, St) ->
    Op = case Ins of
	     ?FOP2_LIST
	     _ -> throw({error, {unknown_fop1, Ins}})
	 end,
    Op1 = case Fs of
	      [copysign_f64] -> Op bor ?SLJIT_COPYSIGN_F64;
	      [copysign_f32] -> Op bor ?SLJIT_COPYSIGN_F32;
	      [] -> Op
	  end,
    %% fixme: emit imm float 
    {Src1, Src1w} = fsrc(S1),
    {Src2, Src2w} = fsrc(S2),
    case fdst(D) of
	{DR, 0} when ?SLJIT_IS_REG(DR) ->
	    ok = emit(Compile, fop2r, 
		      [Op1, DR, Src1, Src1w, Src2, Src2w]),
	    St;
	{Dst, Dstw} ->
	    ok = emit(Compile, fop2, 
		      [Op1,Dst, Dstw, Src1, Src1w, Src2, Src2w]),
	    St
    end.

ins_set_flags([]) -> 0;
ins_set_flags([F|Fs]) ->
    case F of
	less -> ?SLJIT_SET_LESS;
	greater_equal -> ?SLJIT_SET_GREATER_EQUAL;
	greater -> ?SLJIT_SET_GREATER;
	less_equal -> ?SLJIT_SET_LESS_EQUAL;
	sig_less -> ?SLJIT_SET_SIG_LESS;
	sig_greater_equal -> ?SLJIT_SET_SIG_GREATER_EQUAL;
	sig_greater -> ?SLJIT_SET_SIG_GREATER;
	sig_less_equal -> ?SLJIT_SET_SIG_LESS_EQUAL;
	carry -> ?SLJIT_SET_CARRY;
	overflow -> ?SLJIT_SET_OVERFLOW;
	z -> ?SLJIT_SET_Z;
	_ when is_integer(F) -> F
    end bor ins_set_flags(Fs).
	
-spec cmp(sljit:compiler(), {cmp, Test::atom(), S1::imm()|reg(), S2::imm()|reg()}, St::map()) -> sljit:jump().
cmp(Compile, {cmp, Test, S1, S2}, _St) ->
    Type = 
	case Test of
	    %% unsigned compare
	    equal -> ?SLJIT_EQUAL;
	    not_equal -> ?SLJIT_NOT_EQUAL;
	    less -> ?SLJIT_LESS;
	    greater_equal -> ?SLJIT_GREATER_EQUAL;
	    greater -> ?SLJIT_GREATER;
	    less_equal -> ?SLJIT_LESS_EQUAL;
	    %% signed compare
	    sig_less -> ?SLJIT_SIG_LESS;
	    sig_greater_equal -> ?SLJIT_SIG_GREATER_EQUAL;
	    sig_greater -> ?SLJIT_SIG_GREATER;
	    sig_less_equal -> ?SLJIT_SIG_LESS_EQUAL;
	    _ when is_integer(Test) -> Test
	end,
    {Src1, Src1w} = src(S1),
    {Src2, Src2w} = src(S2),
    emit(Compile, cmp, [Type, Src1, Src1w, Src2, Src2w]).

add_target(L, Jump, St) ->
    case maps:find({label,L}, St) of
	{ok, Label} ->
	    sljit:set_label(Jump, Label),
	    St;
	error ->
	    case maps:find({target,L}, St) of
		{ok, Jumps} ->
		    St#{ {target, L} => [Jump|Jumps] };
		error ->
		    St#{ {target, L} => [Jump] }
	    end
    end.
    
ins_label(Compile, {label, L}, St) ->
    case maps:find({label, L}, St) of
	{ok, _Label} ->
	    throw({error, {duplicate_label, L}});
	error ->
	    ok = emit(Compile, label_name, [L]),
	    case maps:find({target, L}, St) of
		{ok, Jumps} ->
		    Label = emit(Compile, label, []),
		    lists:foreach(fun(Jump) -> 
					  %% code generation?
					  sljit:set_label(Jump, Label)
				  end, Jumps),
		    St#{ {label, L} => Label, {target, L} => [] };
		error ->
		    Label = emit(Compile, label, []),
		    St#{ {label, L} => Label }
	    end
    end.

ins_jump(Compile, {jump, {Mod,Fun}}, St) when is_atom(Mod), is_atom(Fun) ->
    ok = emit(Compile, mjump, [?SLJIT_JUMP, Mod, Fun]),
    St;
ins_jump(Compile, {jump, L0}, St) ->  %% Jump always {jump, L0}
    ok = emit(Compile, label_name, [L0]),
    Jump = emit(Compile, jump, [?SLJIT_JUMP]),  %% ?SLJIT_FAST_CALL
    add_target(L0, Jump, St);

ins_jump(Compile, {jump, Type0, L0}, St) when is_atom(Type0) ->
    Type = case Type0 of
	       equal -> ?SLJIT_EQUAL;
	       not_equal -> ?SLJIT_NOT_EQUAL;
	       less -> ?SLJIT_LESS;
	       greater_equal -> ?SLJIT_GREATER_EQUAL;
	       less_equal -> ?SLJIT_LESS_EQUAL;
	       overflow -> ?SLJIT_OVERFLOW;
	       not_overflow -> ?SLJIT_NOT_OVERFLOW;
	       carry -> ?SLJIT_CARRY;
	       not_carry -> ?SLJIT_NOT_CARRY;
	       _ when is_integer(Type0) -> Type0
	   end,
    ok = emit(Compile, label_name, [L0]),
    Jump = emit(Compile, jump, [Type]),  %% ?SLJIT_FAST_CALL
    add_target(L0, Jump, St);
ins_jump(Compile, {jump, Cmp, L0}, St) when %% Jump on compare {jump, {cmp, less, r1, r2}, L0}
      element(1, Cmp) =:= cmp ->
    ok = emit(Compile, label_name, [L0]),
    Jump = cmp(Compile, Cmp, St),
    add_target(L0, Jump, St).

ins_ijump(Compile, {ijump, Type, S}, St) ->
    {Src, Srcw} = src(S),
    Type1 = case Type of
		fast_call ->  ?SLJIT_FAST_CALL;
		jump -> ?SLJIT_JUMP;
		_ when is_integer(Type) -> Type
	    end,
    ok = emit(Compile, ijump, [Type1, Src, Srcw]),
    St.


ins_call(Compile, {call, Type, Ret, Args, {Mod,Fun}}, St) ->
    Type1 = encode_icall_type(Type),
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    ok = emit(Compile, mcall, [Type1, ArgTypes, Mod, Fun]);

ins_call(Compile, {call, Type, Ret, Args, L0}, St) ->
    ok = emit(Compile, label_name, [L0]),
    Type1 = encode_call_type(Type),
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    Jump = sljit:emit_call(Compile, Type1, ArgTypes),
    add_target(L0, Jump, St).

ins_icall(Compile, {icall, Type, Ret, Args, S}, St) ->
    {Src, Srcw} = src(S),
    Type1 = encode_icall_type(Type),
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    ok = emit(Compile, icall, [Type1, ArgTypes, Src, Srcw]),
    St.



ins_enter(Compile, {enter, Options0, RetType0, ArgTypes0, 
		    Scratches0, Saved0, LocalSize}, St) ->
    Options = enter_options(Options0),
    Scratches = enter_regs(Scratches0),
    Saved = enter_regs(Saved0),
    RetType = encode_ret(RetType0),
    ArgTypes = RetType bor encode_args(ArgTypes0),
    ok = emit(Compile, enter,
	      [Options, ArgTypes, Scratches, Saved, LocalSize]),
    St;
ins_enter(Compile, {set_context, Options0, RetType0, ArgTypes0, 
		    Scratches0, Saved0, LocalSize}, St) ->
    Options = enter_options(Options0),
    Scratches = enter_regs(Scratches0),
    Saved = enter_regs(Saved0),
    RetType = encode_ret(RetType0),
    ArgTypes = RetType bor encode_args(ArgTypes0),
    ok = emit(Compile, set_context, 
	      [Options, ArgTypes, Scratches, Saved, LocalSize]),
    St.

ins_return(Compile, {return}, St) ->
    ok = emit(Compile, return_void, []),
    St;
ins_return(Compile, return, St) ->
    ok = emit(Compile, return_void, []),
    St;
ins_return(Compile, {return, Op0, S}, St) ->
    case Op0 of
	mov -> 
	    {Src, Srcw} = src(S),
	    ok = emit(Compile, return, [?SLJIT_MOV, Src, Srcw]);
	mov_p ->
	    {Src, Srcw} = src(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_P, Src, Srcw]);
	mov_f32 ->
	    %% fixme: emit imm float
	    {Src, Srcw} = fsrc(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_F32, Src, Srcw]);
	mov_f64 ->
	    %% fixme: emit imm float
	    {Src, Srcw} = fsrc(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_F64, Src, Srcw]);
	_ when is_integer(Op0) ->
	    {Src, Srcw} = src(S), 
	    ok = emit(Compile, return, [Op0, Src, Srcw])
    end,
    St.

-undef(SIMD_OP2_NAME).
-define(SIMD_OP2_NAME(I, N), I -> N; ).

ins_simd_op2(Compile, {Ins, D, S1, S2}, St) ->
    ins_simd_op2(Compile, {Ins, [], D, S1, S2}, St);
ins_simd_op2(Compile, {Ins, Fs, D, S1, S2}, St) when is_list(Fs) ->
    Op = case Ins of
	     ?SIMD_OP2_LIST
	     _ -> throw({error, {unknown_simd_op2, Ins}})
	 end,
    OpF = ins_set_vflags(Fs),
    DstVReg = vreg(D),
    Src1VReg = vreg(S1),
    {Src2, Src2w} = src(S2),
    ok = emit(Compile, simd_op2, 
	      [Op bor OpF, DstVReg, Src1VReg, Src2, Src2w]),
    St.


ins_set_vflags([]) -> 0;
ins_set_vflags([F|Fs]) ->
    case F of
	float -> ?SLJIT_SIMD_FLOAT;
	test -> ?SLJIT_SIMD_TEST;
	u8 -> ?SLJIT_SIMD_ELEM_8;
	u16 -> ?SLJIT_SIMD_ELEM_16;
	u32 -> ?SLJIT_SIMD_ELEM_32;
	u64 -> ?SLJIT_SIMD_ELEM_64;
	u128 -> ?SLJIT_SIMD_ELEM_128;
	u256 -> ?SLJIT_SIMD_ELEM_256;
	unaligned -> ?SLJIT_SIMD_MEM_UNALIGNED;
	aligned_16 -> ?SLJIT_SIMD_MEM_ALIGNED_16;
	aligned_32 -> ?SLJIT_SIMD_MEM_ALIGNED_32;
	aligned_64 -> ?SLJIT_SIMD_MEM_ALIGNED_64;
	aligned_128 -> ?SLJIT_SIMD_MEM_ALIGNED_128;
	aligned_256 -> ?SLJIT_SIMD_MEM_ALIGNED_256;
	aligned_512 -> ?SLJIT_SIMD_MEM_ALIGNED_512;
	_ when is_integer(F) -> F
    end bor ins_set_vflags(Fs).

ins_fset32(Compile, {fset32, Reg, Value}, St) ->
    ok = emit(Compile, fset32, [freg(Reg), Value]),
    St.

ins_fset64(Compile, {fset64, Reg, Value}, St) ->
    ok = emit(Compile, fset64, [freg(Reg), Value]),
    St.

%% compile and emit binary 
emit({fd,Fd,Compile}, Fmt, Args) ->
    SLFunc = emit_func(Fmt),
    Res = apply(sljit, SLFunc, [Compile|Args]),
    ok = emitf(Fd, Fmt, Args),
    Res;
emit(Compile, Fmt, Args) ->
    SLFunc = emit_func(Fmt),
    apply(sljit, SLFunc, [Compile|Args]).

%% encode and write Fmt Args to file
emitf(Fd, Fmt, Args) ->
    F = encode_fmt(Fmt),
    Sig = fmt_signature(F),
    Bin1 = encode_fmt_args(Sig, Args),
    true = (byte_size(Bin1) < 254),
    Bin = <<(byte_size(Bin1)+1),F,Bin1/binary>>,
    file:write(Fd, Bin).

%% Convert format name into emit function name
emit_func(op0) -> emit_op0;
emit_func(op1) -> emit_op1;
emit_func(op2) -> emit_op2;
emit_func(op2u) -> emit_op2u;
emit_func(op2r) -> emit_op2r;
emit_func(shift_into) -> emit_shift_into;
emit_func(op2r) -> emit_op2r;
emit_func(op2) -> emit_op2;
emit_func(op1) -> emit_op1;
emit_func(fop1) -> emit_fop1;
emit_func(fop2) -> emit_fop2;
emit_func(fop2r) -> emit_fop2r;
emit_func(fset32) -> emit_fset32;
emit_func(fset64) -> emit_fset64;
emit_func(fcopy) -> emit_fcopy;
emit_func(label) -> emit_label;
emit_func(jump) -> emit_jump;
emit_func(cmp) -> emit_cmp;
emit_func(enter) -> emit_enter;
emit_func(call) -> emit_call;
emit_func(icall) -> emit_icall;
emit_func(mcall) -> emit_mcall;
emit_func(mjump) -> emit_mjump;
emit_func(set_context) -> set_context;
emit_func(return_void) -> emit_return_void;
emit_func(return) -> emit_return;
emit_func(simd_op2) -> emit_simd_op2;
emit_func(label_name) -> label_name;
emit_func(module) -> module;
emit_func(function) -> function.


-undef(OP0_NAME).
-define(OP0_NAME(I, N), I -> ?FMT_OP0; ).

-undef(OP1_NAME).
-define(OP1_NAME(I, N), I -> ?FMT_OP1; ).

-undef(OP2_NAME).
-define(OP2_NAME(I, N), I -> ?FMT_OP2; ).

-undef(FOP1_NAME).
-define(FOP1_NAME(I, N), I -> ?FMT_FOP1; ).

-undef(FOP2_NAME).
-define(FOP2_NAME(I, N), I -> ?FMT_FOP2; ).

-undef(FOP2R_NAME).
-define(FOP2R_NAME(I, N), I -> ?FMT_FOP2R; ).

-undef(OP_FCOPY_NAME).
-define(OP_FCOPY_NAME(I, N), I -> ?FMT_FCOPY; ).

-undef(OP_SI_NAME).
-define(OP_SI_NAME(I, N), I -> ?FMT_SI; ).

-undef(SIMD_OP2_NAME).
-define(SIMD_OP2_NAME(I, N), I -> ?FMT_SIMD_OP2; ).

fmt(Ins) ->
    case opcode(Ins) of
	?OP0_LIST
	?OP1_LIST
	?OP2_LIST
	?FOP1_LIST
	?FOP2_LIST
	?OP_FCOPY_LIST
	%% ?OP_SI_LIST
	label -> ?FMT_LABEL;
	jump -> ?FMT_JUMP;
	enter -> ?FMT_ENTER;
	set_context -> ?FMT_SET_CONTEXT;
	return -> ?FMT_RETURN;
	return_void -> ?FMT_RETURN;
	label_name -> ?FMT_LABEL_NAME;
	module -> ?FMT_MODULE;
	function -> ?FMT_FUNCTION;
	fset32 -> ?FMT_FSET32;
	fset64 -> ?FMT_FSET64;
	call -> ?FMT_CALL;
	icall -> ?FMT_ICALL;
	ijump -> ?FMT_IJUMP;
	?SIMD_OP2_LIST
	_ -> throw({error, {unknown_op, Ins}})
    end.

-undef(FMT_NAME).
-define(FMT_NAME(Name, Value), Name -> Value; ).

%% encode emit_<fmt> (fixme?)
encode_fmt(Fmt) ->
    case Fmt of
	?FMT_LIST
	_ -> throw({error, {unknown_format, Fmt}})
    end.

-undef(FMT_NAME).
-define(FMT_NAME(Name, Value), Value -> Name; ).

decode_fmt(Fmt) ->
    case Fmt of
	?FMT_LIST
	_ -> throw({error, {unknown_format, Fmt}})
    end.

%% format signature number and types of arguments
fmt_signature(Fmt) ->
    case Fmt of 
	?FMT_OP0 -> "u";
	?FMT_OP1 -> "uuiui";
	?FMT_OP2 -> "uuiuiui";
	?FMT_OP2U -> "uuiui";
	?FMT_OP2R -> "uuuiui";
	?FMT_SI -> "uuuuui";
	?FMT_FOP1 -> "uuiui";
	?FMT_FOP2 -> "uuiuiui";
	?FMT_FOP2R -> "uuuiui";
	?FMT_FSET32 -> "uf";
	?FMT_FSET64 -> "ud";
	?FMT_FCOPY -> "uuu";
	?FMT_LABEL_NAME -> "v";
	?FMT_MODULE -> "s";
	?FMT_FUNCTION -> "s";
	?FMT_LABEL -> "";
	?FMT_CMP -> "uuiui";
	?FMT_JUMP -> "u";
	?FMT_ENTER -> "uuuuu";
	?FMT_SET_CONTEXT -> "uuuuu";
	?FMT_RETURN_VOID -> "";
	?FMT_RETURN -> "uui";
	?FMT_SIMD_OP2 -> "uuuui";
	?FMT_MCALL -> "uuss";
	?FMT_MJUMP -> "uss";
	?FMT_ICALL -> "uuui";
	?FMT_IJUMP -> "uui";
	?FMT_CALL -> "uu"
    end.


opcode(T) when is_atom(T) ->
    T;
opcode(T) when is_atom(element(1, T)) ->
    element(1, T).

-spec dst(imm()|mem()|reg()) -> {sljit:op_src(), integer()}.
dst({mem,Imm}) when is_integer(Imm) ->
    {?SLJIT_MEM0(), Imm};
dst({mem,R1}) -> 
    {?SLJIT_MEM1(reg(R1)), 0};
dst({mem,R1,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM1(reg(R1)), Imm};
dst({mem,R1,R2}) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), 0};
dst({mem,R1,R2,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), Imm};
dst({reg, R}) -> 
    {reg(R), 0};
dst(R) when is_atom(R) ->
    {reg(R), 0}.

-spec src(imm()|mem()|reg()|integer()) -> {sljit:op_src(), integer()}.
src(Imm) when is_integer(Imm) ->
    {?SLJIT_IMM, Imm};
src({imm,Imm}) when is_integer(Imm) ->
    {?SLJIT_IMM, Imm};
src({mem,Imm}) when is_integer(Imm) ->
    {?SLJIT_MEM0() bor ?SLJIT_IMM, Imm};
src({mem,R1}) -> 
    {?SLJIT_MEM1(reg(R1)), 0};
src({mem,R1,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM1(reg(R1)), Imm};
src({mem,R1,R2}) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), 0};
src({mem,R1,R2,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), Imm};
src({reg, R}) -> 
    {reg(R), 0};
src(R) when is_atom(R) ->
    {reg(R), 0}.


-spec fdst(imm()|mem()|freg()) -> {sljit:op_src(), integer()}.
fdst({mem,Imm}) when is_integer(Imm) ->
    {?SLJIT_MEM0(), Imm};
fdst({mem,R1}) -> 
    {?SLJIT_MEM1(reg(R1)), 0};
fdst({mem,R1,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM1(reg(R1)), Imm};
fdst({mem,R1,R2}) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), 0};
fdst({mem,R1,R2,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), Imm};
fdst({freg, R}) -> 
    {freg(R), 0};
fdst(R) when is_atom(R) ->
    {freg(R), 0}.

-spec fsrc(imm()|mem()|freg()|integer()) -> {sljit:op_src(), integer()}.
fsrc(Imm) when is_float(Imm) ->
    {?SLJIT_IMM, Imm};
fsrc({imm,Imm}) when is_float(Imm) ->
    {?SLJIT_IMM, Imm};
fsrc({mem,Imm}) when is_integer(Imm) ->
    {?SLJIT_MEM0() bor ?SLJIT_IMM, Imm};
fsrc({mem,R1}) -> 
    {?SLJIT_MEM1(reg(R1)), 0};
fsrc({mem,R1,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM1(reg(R1)), Imm};
fsrc({mem,R1,R2}) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), 0};
fsrc({mem,R1,R2,Imm}) when is_integer(Imm) -> 
    {?SLJIT_MEM2(reg(R1), reg(R2)), Imm};
fsrc({freg, R}) -> 
    {freg(R), 0};
fsrc(R) when is_atom(R) ->
    {freg(R), 0}.

enter_options([]) -> 0;
enter_options([{keep,N}|Opts]) ->
    ?SLJIT_ENTER_KEEP(N) bor enter_options(Opts);
enter_options([reg_arg |Opts]) ->
    ?SLJIT_ENTER_REG_ARG bor enter_options(Opts).

enter_regs([]) -> 0;     
enter_regs([{reg,R}|Regs]) ->  R bor enter_regs(Regs);
enter_regs([{freg,R}|Regs]) -> ?SLJIT_ENTER_FLOAT(R) bor enter_regs(Regs);
enter_regs([{vreg,R}|Regs]) -> ?SLJIT_ENTER_VECTOR(R) bor enter_regs(Regs).

encode_ret(Ret) ->
    R = case Ret of
	    void   -> ?SLJIT_ARG_TYPE_RET_VOID;
	    word   -> ?SLJIT_ARG_TYPE_W;
	    word32 -> ?SLJIT_ARG_TYPE_32;
	    ptr    -> ?SLJIT_ARG_TYPE_P;
	    f64    -> ?SLJIT_ARG_TYPE_F64;
	    f32    -> ?SLJIT_ARG_TYPE_F32;
	    _ when is_integer(Ret) -> Ret
	end,
    ?SLJIT_ARG_RETURN(R).
    
%% enter argtypes
encode_args(As) ->
    encode_args_(1, As).

encode_args_(_I, []) -> 0;
encode_args_(I, [Arg|As]) ->
    A = encode_arg(Arg),
    ?SLJIT_ARG_VALUE(A,I) bor encode_args_(I+1, As).

encode_arg(Arg) ->
    case Arg of
	word -> ?SLJIT_ARG_TYPE_W_R;
	word32 -> ?SLJIT_ARG_TYPE_32_R;
	ptr -> ?SLJIT_ARG_TYPE_P_R;
	f64 -> ?SLJIT_ARG_TYPE_F64;
	f32 -> ?SLJIT_ARG_TYPE_F32;
	_ when is_integer(Arg) -> Arg
    end.

%% ICALL type
encode_icall_type([call|Ts]) ->
    ?SLJIT_CALL bor encode_icall_type_(Ts);
encode_icall_type([call_reg_arg|Ts]) -> 
    ?SLJIT_CALL_REG_ARG bor encode_icall_type_(Ts);
encode_icall_type([]) ->  %% src,srcw IMM and address
    0;
encode_icall_type(Type) when is_integer(Type) ->
    Type.

encode_icall_type_([call_return|Ts]) ->
    ?SLJIT_CALL_RETURN bor encode_icall_type_(Ts);
encode_icall_type_([]) ->
    0.

encode_call_type([call|Ts]) ->
    ?SLJIT_CALL bor encode_call_type_(Ts);
encode_call_type([call_reg_arg|Ts]) -> 
    ?SLJIT_CALL_REG_ARG bor encode_call_type_(Ts);
encode_call_type(Type) when is_integer(Type) ->
    Type.

encode_call_type_([rewriteable_jump|Ts]) ->
    ?SLJIT_REWRITABLE_JUMP bor encode_call_type_(Ts);
encode_call_type_([call_return|Ts]) ->
    ?SLJIT_CALL_RETURN bor encode_call_type_(Ts);
encode_call_type_([]) ->
    0.


reg(r0) -> ?SLJIT_R0;
reg(r1) -> ?SLJIT_R1;
reg(r2) -> ?SLJIT_R2;
reg(r3) -> ?SLJIT_R3;
reg(r4) -> ?SLJIT_R4;
reg(r5) -> ?SLJIT_R5;
reg(r6) -> ?SLJIT_R6;
reg(r7) -> ?SLJIT_R7;
reg(r8) -> ?SLJIT_R8;
reg(r9) -> ?SLJIT_R9;
reg({r,I}) -> ?SLJIT_R(I);
reg(s0) -> ?SLJIT_S0;
reg(s1) -> ?SLJIT_S1;
reg(s2) -> ?SLJIT_S2;
reg(s3) -> ?SLJIT_S3;
reg(s4) -> ?SLJIT_S4;
reg(s5) -> ?SLJIT_S5;
reg(s6) -> ?SLJIT_S6;
reg(s7) -> ?SLJIT_S7;
reg(s8) -> ?SLJIT_S8;
reg(s9) -> ?SLJIT_S9;
reg({s,I}) -> ?SLJIT_S(I).

freg(fr0) -> ?SLJIT_FR0;
freg(fr1) -> ?SLJIT_FR1;
freg(fr2) -> ?SLJIT_FR2;
freg(fr3) -> ?SLJIT_FR3;
freg(fr4) -> ?SLJIT_FR4;
freg(fr5) -> ?SLJIT_FR5;
freg(fr6) -> ?SLJIT_FR6;
freg(fr7) -> ?SLJIT_FR7;
freg(fr8) -> ?SLJIT_FR8;
freg(fr9) -> ?SLJIT_FR9;
freg({fr,I}) -> ?SLJIT_FR(I);
freg(fs0) -> ?SLJIT_FS0;
freg(fs1) -> ?SLJIT_FS1;
freg(fs2) -> ?SLJIT_FS2;
freg(fs3) -> ?SLJIT_FS3;
freg(fs4) -> ?SLJIT_FS4;
freg(fs5) -> ?SLJIT_FS5;
freg(fs6) -> ?SLJIT_FS6;
freg(fs7) -> ?SLJIT_FS7;
freg(fs8) -> ?SLJIT_FS8;
freg(fs9) -> ?SLJIT_FS9;
freg({fs,I}) -> ?SLJIT_FS(I).

vreg(vr0) -> ?SLJIT_VR0;
vreg(vr1) -> ?SLJIT_VR1;
vreg(vr2) -> ?SLJIT_VR2;
vreg(vr3) -> ?SLJIT_VR3;
vreg(vr4) -> ?SLJIT_VR4;
vreg(vr5) -> ?SLJIT_VR5;
vreg(vr6) -> ?SLJIT_VR6;
vreg(vr7) -> ?SLJIT_VR7;
vreg(vr8) -> ?SLJIT_VR8;
vreg(vr9) -> ?SLJIT_VR9;
vreg({vr,I}) -> ?SLJIT_VR(I);
vreg(vs0) -> ?SLJIT_VS0;
vreg(vs1) -> ?SLJIT_VS1;
vreg(vs2) -> ?SLJIT_VS2;
vreg(vs3) -> ?SLJIT_VS3;
vreg(vs4) -> ?SLJIT_VS4;
vreg(vs5) -> ?SLJIT_VS5;
vreg(vs6) -> ?SLJIT_VS6;
vreg(vs7) -> ?SLJIT_VS7;
vreg(vs8) -> ?SLJIT_VS8;
vreg(vs9) -> ?SLJIT_VS9;
vreg({vs,I}) -> ?SLJIT_VS(I).

%%
%% Encode arguments using function signature
%%
encode_fmt_args(Sig, Args) ->
    iolist_to_binary(encode_fmt_args_(Sig, Args)).

encode_fmt_args_([$u|Sig], [A|Args]) when is_integer(A), A >= 0 ->
    [encode_fmt_uarg_(A,[]) | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$i|Sig], [A|Args]) when is_integer(A) ->
    [encode_fmt_arg(A) | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$f|Sig], [A|Args]) when is_float(A) ->
    [<<A:32/float>> | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$d|Sig], [A|Args]) when is_float(A) ->
    [<<A:64/float>> | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$s|Sig], [A|Args]) when is_binary(A); is_list(A) ->
    Bin = iolist_to_binary(A),
    [Bin,0 | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$s|Sig], [A|Args]) when is_atom(A) ->
    Bin = atom_to_binary(A),
    [Bin,0 | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([$v|Sig], [A|Args]) ->
    Bin = term_to_binary(A),
    Sz = byte_size(Bin),
    true = (Sz < 256),
    [(Sz) band 16#ff, Bin | encode_fmt_args_(Sig, Args)];
encode_fmt_args_([], []) -> [].
    
    %% encode integer
%%  6 bit     0:1,S:0,V0:6
%% 13 bit     1:1,S:1:V1:6, 0:1,V0:7
%% 20 bit     1:1,S:1:V2:6, 1:1,V1:7, 0:1,V0:7
%% 6+7*n bit
encode_fmt_arg(X) when is_integer(X) ->
    if X >= 0 ->
	    encode_fmt_uarg_(X,[]);
       true ->
	    encode_fmt_sarg_(X,[])
    end.

encode_fmt_uarg_(X,Acc) ->
    C = if (Acc =/= []) -> 16#80; true -> 16#00 end,
    if X > 16#3f -> %% need continuation
	    encode_fmt_uarg_(X bsr 7, [(C bor (X band 16#7f)) | Acc]);
       true -> %% last byte
	    list_to_binary([(C bor (X band 16#3f)) | Acc])
    end.

encode_fmt_sarg_(X,Acc) ->
    C = if (Acc =/= []) -> 16#80; true -> 16#00 end,
    if X < -16#40 orelse X > 16#3f -> %% need continuation
	    encode_fmt_sarg_(X bsr 7, [(C bor (X band 16#7f)) | Acc]);
       true -> %% last byte
	    list_to_binary([((C bor 16#40) bor (X band 16#3f)) | Acc])
    end.

decode_fmt_args(Sig, Bin) ->
    decode_fmt_args(Sig, Bin, []).

decode_fmt_args([$u|Sig], Bin, Acc) ->
    {Arg, Bin1} = decode_fmt_arg(Bin),
    decode_fmt_args(Sig, Bin1, [Arg | Acc]);
decode_fmt_args([$i|Sig], Bin, Acc) ->
    {Arg, Bin1} = decode_fmt_arg(Bin),
    decode_fmt_args(Sig, Bin1, [Arg | Acc]);
decode_fmt_args([$f|Sig], <<Arg:32/float,Bin/binary>>, Acc) ->
    decode_fmt_args(Sig, Bin, [Arg | Acc]);
decode_fmt_args([$d|Sig], <<Arg:64/float,Bin/binary>>, Acc) ->
    decode_fmt_args(Sig, Bin, [Arg | Acc]);
decode_fmt_args([$s|Sig], Bin, Acc) ->
    {Arg,Bin1} = decode_string(Bin, []),
    decode_fmt_args(Sig, Bin1, [Arg | Acc]);
decode_fmt_args([$v|Sig], <<Sz,Bin:Sz/binary,Bin1/binary>>, Acc) ->
    Arg = binary_to_term(Bin),
    decode_fmt_args(Sig, Bin1, [Arg | Acc]);
decode_fmt_args([], <<>>, Acc) ->
    lists:reverse(Acc).

decode_string(<<C,Bin/binary>>, Acc) ->
    if C =:= 0 -> {lists:reverse(Acc), Bin};
       true -> decode_string(Bin, [C | Acc])
    end.

decode_fmt_arg(<<0:1,S:1,Vn:6, Bin/binary>>) ->
    if S =:= 0 -> %% non-negative >= 0
	    {Vn, Bin};
       true -> %% negative
	    {(Vn-64), Bin}
    end;
decode_fmt_arg(<<1:1,S:1,Vn:6, Bin/binary>>) ->
    if S =:= 0 -> %% non-negative >= 0
	    decode_fmt_arg_(Bin, Vn);
       true -> %% negative
	    decode_fmt_arg_(Bin, (Vn-64))
    end.

decode_fmt_arg_(<<0:1,V:7,Bin/binary>>, V0) ->
    {(V0 bsl 7) bor V, Bin};
decode_fmt_arg_(<<1:1,V:7,Bin/binary>>, V0) ->
    decode_fmt_arg_(Bin, (V0 bsl 7) bor V).
