%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%     Assembler for symbolic SLJIT instructions
%%% @end
%%% Created : 19 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit_asm).

-export([assemble/1, assemble/2]).
-export([assemble_list/1, assemble_list/2, assemble_list/3]).
-export([assemble_file/2, assemble_file/3]).
-export([disasm/1, disasm/2, disasm/3]).
-export([load/1]).
-export([save_as_bin/2]).
-export([asm_ins_list/3, asm_ins/3]).
-export([slo_ins_list/3, slo_ins/3]).

-type imm() :: sljit:imm().
%%-type mem() :: sljit:mem().
-type reg() :: sljit:reg().
-type freg() :: sljit:freg().
-type vreg() :: sljit:vreg().

-type src() :: sljit:src().
-type dst() :: sljit:dst().

-type fsrc() :: sljit:fsrc().
-type fdst() :: sljit:fdst().

-type vsrc() :: sljit:vsrc().
-type vdst() :: sljit:vdst().
-type vsrcdst() :: sljit:vsrcdst().


%% debug
-compile(export_all).

-include("../include/sljit.hrl").
-include("../include/sljit_asm.hrl").

-define(compare_integer_type(X),
	((X) >= ?SLJIT_EQUAL) 
	andalso
	  ((X) =< ?SLJIT_SIG_LESS_EQUAL)).

-define(compare_float_type(X),
	((X) >= ?SLJIT_F_EQUAL) 
	andalso
	  ((X) =< ?SLJIT_F_LESS_EQUAL)).

%% -define(dbg(Fmt, Args), io:format(Fmt, Args)).
-define(dbg(Fmt, Args), ok).

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
	mlshr | mlshr32 | lshr | lshr32.

-type op_src() :: fast_return | skip_frames_before_fast_return |
		  prefetch_l1 | prefetch_l2 | prefetch_l3 | prefetch_once.

-type op_dst() :: fast_enter | get_return_address.

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

-type simd_mov() :: vload | vstore.

-export_type([op0/0, op1/0, op2/0, op2r/0, op_shift_into/0, op_src/0, op_dst/0]).
-export_type([fop1/0, fop2/0, fop2r/0, op_fcopy/0, simd_op2/0, simd_mov/0]).

-define(test_flag(F, X), ((X) band (F)) =:= (F)).

%% Assemble a file.asm into slo object binary
-spec assemble(Filename::string()) -> {ok, binary()} | {error, term()}.
assemble(Filename) ->
    assemble(native, Filename).

-spec assemble(Arch::sljit:arch(), Filename::string()) ->
	  {ok, binary()} | {error, term()}.
assemble(Arch, Filename) ->
    case file:consult(Filename) of
	{ok, Instructions} ->
	    case file:open(<<>>, [ram, binary, write]) of
		{ok, Fd} ->
		    Compile = sljit:create_compiler(Arch),
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

-spec assemble_file(Filename::string(), DstFilename::string()) -> 
	  ok | {error, term()}.
assemble_file(Filename, DstFilename) ->
    assemble_file(native, Filename, DstFilename).

-spec assemble_file(Arch::sljit:arch(),Filename::string(), DstFilename::string()) -> 
	  ok | {error, term()}.
assemble_file(Arch, Filename, DstFilename) ->
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
		    Compile = sljit:create_compiler(Arch),
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

%% Assemble a file.asm into machine code binary

-spec assemble_list(Instuctions::[term()]) -> 
	  {ok, binary()} | {error, term()}.
assemble_list(Instructions) when is_list(Instructions) ->
    assemble_list(aut, Instructions).

-spec assemble_list(Arch::sljit:arch(),Instuctions::[term()]) -> 
	  {ok, binary()} | {error, term()}.
assemble_list(Arch,Instructions) when is_list(Instructions) ->
    Compile = sljit:create_compiler(Arch),
    _Sym = asm_ins_list(Compile, Instructions, #{}),
    {_,Code} = sljit:generate_code(Compile),
    Bin = sljit:get_code(Code),
    {ok, Bin}.

-spec assemble_list(Arch::sljit:arch(),Instuctions::[term()],Filename::string()) ->
	  ok | {error, term()}.
assemble_list(Arch,Instructions,Filename) when is_list(Instructions) ->
    {ok,Bin} = assemble_list(Arch,Instructions),
    file:write_file(Filename, Bin).

disasm(Filename) ->
    disasm(Filename, undefined, []).
disasm(Filename, Output) ->
    disasm(Filename, Output, ok).
disasm(Filename, Output, Acc) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    if 
		Output =:= undefined ->
		    disasm_object(Bin, #{}, Output, Acc);
		Output =:= standard_io;
		Output =:= standard_error;
		Output =:= user;
		element(1, Output) =:= file_descriptor;
		is_pid(Output) ->
		    disasm_object(Bin, #{}, Output, Acc);
		is_binary(Output); is_list(Output) ->
		    {ok, Fd} = file:open(Output, [raw,read,write,trunc]),
		    try disasm_object(Bin, #{}, Fd, Acc) of
			Ret -> Ret
		    after
			file:close(Fd)
		    end
	    end;
	Error ->
	    Error
    end.

disasm_object(<<>>, _St, _Fd, ok) ->
    ok;
disasm_object(<<>>, _St, _Fd, Acc) ->
    {ok, lists:reverse(Acc)};
disasm_object(<<Sz, Data:Sz/binary, Rest/binary>>, St, Out, Acc) ->
    {Ins0,Sz} = binary_to_term(Data, [used]),
    case disasm_ins(Ins0, St) of
	{nop, St1} ->
	    disasm_object(Rest, St1, Out, Acc);
	{Ins, St1} ->
	    print_object(Ins, Out),
	    if is_list(Acc) ->
		    disasm_object(Rest, St1, Out, [Ins|Acc]);
	       true ->
		    disasm_object(Rest, St1, Out, Acc)
	    end
    end.

print_object(_, undefined) ->
    ok;
print_object(Ins, Fd) ->
    case element(1, Ins) of
	module -> io:format(Fd, "~p.\n", [Ins]);
	function -> io:format(Fd, "~p.\n", [Ins]);
	label -> io:format(Fd, "~p.\n", [Ins]);
	_ -> io:format(Fd, "  ~p.\n", [Ins])
    end.

-undef(OP0_NAME).
-define(OP0_NAME(I, N), N -> I; ).

-undef(OP1_NAME).
-define(OP1_NAME(I, N), N -> I; ).

-undef(OP2_NAME).
-define(OP2_NAME(I, N), N -> I; ).

-undef(OP2R_NAME).
-define(OP2R_NAME(I, N), N -> I; ).

-undef(OP_SRC_NAME).
-define(OP_SRC_NAME(I, N), N -> I; ).

-undef(OP_DST_NAME).
-define(OP_DST_NAME(I, N), N -> I; ).

-undef(FOP1_NAME).
-define(FOP1_NAME(I, N), N -> I; ).

-undef(FOP2_NAME).
-define(FOP2_NAME(I, N), N -> I; ).

-undef(FOP2R_NAME).
-define(FOP2R_NAME(I, N), N -> I; ).

-undef(OP_FCOPY_NAME).
-define(OP_FCOPY_NAME(I, N), N -> I; ).

-undef(OP_SHIFT_INTO_NAME).
-define(OP_SHIFT_INTO_NAME(I, N), N -> I; ).

-undef(SIMD_OP2_NAME).
-define(SIMD_OP2_NAME(I, N), N -> I; ).

-undef(SIMD_MOV_NAME).
-define(SIMD_MOV_NAME(I, N), N -> I; ).

disasm_ins(Slo, St) ->
    case Slo of
	{label_name,[Name]} ->
	    {nop, St#{label_name => Name}};
	{jump_name,[Name]} ->
	    {nop, St#{jump_name => Name}};
	{const_name,[Name]} ->
	    {nop, St#{const_name => Name}};
	{module,[Name]} ->
	    {{module, Name}, St};
	{function,[Name]} ->
	    {{function,Name},St};
	{op0,[Op]} -> 
	    OpName = case Op of
		?OP0_LIST
		_ -> throw({error, {unknown_op, Op}})
	    end,
	    {{OpName}, St};
	{op1,[Op,D,S]} ->
	    OpName = case Op of
		?OP1_LIST
		_ -> throw({error, {unknown_op, Op}})
	    end,
	    {{OpName,D,S},St};
	{op2,[Op,D,S1,S2]} ->
	    OpName = 
		case Op of
		    ?OP2_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,D,S1,S2},St};
	{op2u,[Op,D,S1,S2]} ->
	    OpName = 
		case Op of
		    ?OP2_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,D,S1,S2},St};
	{op2r,[Op,D,S1,S2]} ->
	    OpName = 
		case Op of
		    ?OP2R_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,D,S1,S2},St};
	{shift_into, [Op, D, S1, S2, S3]} ->
	    OpName = 
		case Op of
		    ?OP_SHIFT_INTO_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,D,S1,S2,S3},St};
	{op_src, [Op,Src]} ->
	    OpName = 
		case Op of
		    ?OP_SRC_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,Src},St};
	{op_dst, [Op,D]} ->
	    OpName = 
		case Op of
		    ?OP_DST_LIST
		    _ -> throw({error, {unknown_op, Op}})
		end,
	    {{OpName,D},St};
	{fop1, [Op,D,S]} ->
	    OpName = case Op of
			 ?FOP1_LIST
			 _ -> throw({error, {unknown_fop, Op}})
		     end,
	    {{OpName,D,S},St};
	{fop2, [Op, D, S1, S2]} ->
	    OpName = case Op of
			 ?FOP2_LIST
			 _ -> throw({error, {unknown_fop, Op}})
		     end,
	    {{OpName,D,S1,S2},St};
	{fop2r, [Op, D, S1, S2]} ->
	    OpName =
		case Op of
		    ?FOP2_LIST
		    _ -> throw({error, {unknown_fop, Op}})
		end,
	    {{OpName, D, S1, S2}, St};
	{fset32, [FReg, Value]} ->
	    {{fset32, FReg, Value}, St};
	{fset64, [FReg, Value]} ->
	    {{fset64, FReg, Value}, St};
	{fcopy, [Op,FReg,Reg]} ->
	    {{Op,FReg,Reg},St};
	{label, []} -> 
	    {{label, maps:get(label_name, St)}, St};
	{cmp, [Type, S1, S2]} ->
	    L = maps:get(label_name, St),
	    T = dec_cmp(Type),
	    {Opts0,St1} = dec_jump_src(Type, St),
	    {{jump,[{T, S1,S2}|Opts0],L}, St1};
	{jump, [Type]} ->
	    L = maps:get(label_name, St),
	    T = dec_status(Type),
	    {Opts0,St1} = dec_jump_src(Type, St),
	    {{jump, [T|Opts0], L}, St1};
	{fcmp, [Type, S1, S2]} ->
	    L = maps:get(label_name, St),
	    {{Type,S1,S2, L}, St};
	
	{ijump,[Type, S]} ->
	    {{ijump, decode_ijump_type(Type), S}, St};
	{mjump,[Type, Mod, Fun]} ->
	    {{mjump, Type, {Mod, Fun}}, St};
	{call, [Type, ArgTypes]} ->
	    L = maps:get(label_name, St),
	    CallType = decode_call_type(Type),
	    Ret = decode_ret(ArgTypes),
	    As  = decode_args(ArgTypes),
	    {{call, CallType, Ret, As, L}, St};
	{icall, [Type,ArgTypes,Src]} ->
	    CallIType = decode_icall_type(Type),
	    Ret = decode_ret(ArgTypes),
	    As  = decode_args(ArgTypes),
	    {{icall, CallIType, Ret, As, Src}, St};
	%% like ICALL but with mod:fun instead
	{mcall, [Type,ArgTypes,Mod,Fun]} ->
	    ICallType = decode_icall_type(Type),
	    Ret = decode_ret(ArgTypes),
	    As  = decode_args(ArgTypes),
	    MF = {Mod, Fun},
	    {{call, ICallType, Ret, As, MF},St};
	{enter,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    Ret = decode_ret(ArgTypes),
	    As  = decode_args(ArgTypes),
	    Options1 = decode_enter_options(Options),
	    Scratches1 = decode_enter_regs(Scratches),
	    Saved1 = decode_enter_regs(Saved),
	    {{enter,Options1,Ret,As,Scratches1,Saved1,LocalSize}, St};
	{set_context,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    Ret = decode_ret(ArgTypes),
	    As  = decode_args(ArgTypes),
	    Options1 = decode_enter_options(Options),
	    Scratches1 = decode_enter_regs(Scratches),
	    Saved1 = decode_enter_regs(Saved),
	    {{set_context,Options1,Ret,As,Scratches1,Saved1,LocalSize}, St};
	{return_void,[]} ->
	    {{return}, St};
	{return,[Op,Src]} ->
	    OpName = 
		case Op of
		    ?SLJIT_MOV -> mov;
		    ?SLJIT_MOV_P -> mov_p;
		    ?SLJIT_MOV_F32 -> mov_f32;
		    ?SLJIT_MOV_F64 -> mov_f64
		end,
	    {{return,OpName,Src}, St};
	{simd_op2,[Op, Dst, Src1, Src2]} ->
	    OpName = 
		case Op band 16#ff of
		    ?SIMD_OP2_LIST
		    _ -> throw({error, {unknown_simd_op2, Op}})
		end,
	    Flags = decode_simd_op2_flags(Op),
	    {{OpName,Flags,Dst,Src1,Src2},St};
	{simd_mov,[Op, VReg, SrcDst]} ->
	    OpName = 
		case Op band 16#ff of
		    ?SIMD_MOV_LIST
		    _ -> throw({error, {unknown_simd_mov, Op}})
		end,
	    Flags = decode_simd_mov_flags(Op),
	    {{OpName,Flags,VReg,SrcDst},St};
	{const, [Op, Dst, InitValue]} ->
	    Name = maps:get(const_name, St),
	    St1 = maps:remove(const_name, St),
	    {{const, Name, Op, Dst, InitValue}, St1};
	{op_addr, [Op,Dst]} ->
	    L0 = maps:get(label_name, St, undefined),
	    Jsrc = maps:get(jump_name, St, undefined),
	    St1 = maps:remove(jump_name, St),
	    case Op of
		mov_addr -> mov_addr;
		mov_abs_addr -> mov_abs_addr;
		add_abs_addr -> add_abs_addr
	    end,
	    {{op_addr, Jsrc, Op, Dst, L0}, St1};
	{Fmt,_Args} ->
	    throw({error, {unknown_format, Fmt}})
    end.	

save_as_bin(Code, DstFilename) ->
    DstFilename1 = case filename:extension(DstFilename) of
		       ".bin" -> DstFilename;
		       _ -> DstFilename ++ ".bin"
		   end,    
    Bin = sljit:get_code(Code),
    file:write_file(DstFilename1, Bin).

-spec load(Filename::string()) ->
	  {ok,binary()} | {error, Reason::term()}.
load(Filename) ->
    load(native,Filename).

-spec load(Arch::sljit:arch(),Filename::string()) ->
	  {ok,binary()} | {error, Reason::term()}.
load(Arch,Filename) ->
    case filename:extension(Filename) of
	".asm" ->
	    case file:consult(Filename) of
		{ok, InsList} ->
		    Compile = sljit:create_compiler(Arch),
		    _St = asm_ins_list(Compile, InsList, #{}),
		    sljit:generate_code(Compile);
		Error ->
		    Error
	    end;
	".slo" ->
	    case load_slo(Filename) of
		{ok, Slo} ->
		    Compile = sljit:create_compiler(Arch),
		    _St = slo_ins_list(Compile, Slo, #{}),
		    sljit:generate_code(Compile);
		Error ->
		    Error
	    end;
	_ ->
	    {error, {unknown_file_type, Filename}}
    end.

load_slo(File) ->
    load_slo(native,File).
load_slo(Arch,File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Compile = sljit:create_compiler(Arch),
	    load_object(Compile, Bin, []);
	Error ->
	    Error
    end.

load_object(_, <<>>, Acc) ->
    {ok, lists:reverse(Acc)};
load_object(Compile, <<Sz, Data:Sz/binary, Rest/binary>>, Acc) ->
    {Ins,Sz} = binary_to_term(Data, [used]),
    load_object(Compile, Rest, [Ins|Acc]).


slo_ins_list(Compile, [Ins|InsList], Sym) ->
    Sym1 = slo_ins(Compile, Ins, Sym),
    slo_ins_list(Compile, InsList, Sym1);
slo_ins_list(_, [], Sym) ->
    Sym.
    
slo_ins(Compile, Ins, St) ->
    ?dbg("SLO ~w\n", [Ins]),
    case Ins of
	{module, [Mod]} ->  %% synthetic
	    ok = sljit:module(Compile, Mod),
	    St#{module => Mod};
	{function, [Fun]} ->  %% synthetic
	    Label = sljit:function(Compile, Fun),
	    St1 = ins_func(Compile, Fun, Label, St),
	    St1#{function => Fun};
	{label_name, [L]} ->  %% synthetic
	    St#{label_name => L};
	{jump_name, [L]} ->  %% synthetic
	    St#{jump_name => L};
	{op0,[Op]} -> 
	    ok = sljit:emit_op0(Compile, Op),
	    St;
	{op1, [Op, Dst, Src]} ->
	    ok = sljit:emit_op1(Compile, Op, Dst, Src),
	    St;
	{op2, [Op, Dst, Src1, Src2]} ->
	    %% FIXME: add flags
	    ok =sljit:emit_op2(Compile, Op, Dst, Src1, Src2),
	    St;
	{op2u, [Op, Src1, Src2]} ->
	    ok = sljit:emit_op2u(Compile, Op, Src1, Src2),
	    St;
	{op2r, [Op, DR, Src1, Src2]} ->
	    ok = sljit:emit_op2r(Compile, Op, DR, Src1, Src2),
	    St;
	{shift_into, [Op, Dst, Src1, Src2, Src3]} ->
	    ok = sljit:emit_shift_into(Compile, Op, Dst, Src1,Src2, Src3),
	    St;       
	{op_src, [Op, Src]} ->
	    ok = sljit:emit_op_src(Compile, Op, Src),
	    St;
	{op_dst, [Op, Dst]} ->
	    ok = sljit:emit_op_dst(Compile, Op, Dst),
	    St;	    
	{fop1, [Op, Dst, Src]} ->
	    ok = sljit:emit_fop1(Compile, Op, Dst, Src),
	    St;
	{fop2, [Op, Dst, Src1, Src2]} ->
	    ok = sljit:emit_fop2(Compile, Op, Dst, Src1,Src2),
	    St;
	{fop2r, [Op, DR, Src1, Src2]} ->
	    ok = sljit:emit_fop2r(Compile, Op, DR, Src1, Src2),
	    St;
	{fset32, [FReg, Value]} ->
	    ok = sljit:emit_fset32(Compile, FReg, Value),
	    St;
	{fset64, [FReg, Value]} ->
	    ok = sljit:emit_fset64(Compile, FReg, Value),
	    St;
	{fcopy, [Op,FReg,Reg]} ->
	    ok = sljit:emit_fopy(Compile, Op, FReg, Reg),
	    St;
	{label, []} -> 
	    L = maps:get(label_name, St),
	    ins_label(Compile, {label, L}, St);
	{cmp, [Type, Src1, Src2]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_cmp(Compile, Type, Src1, Src2),
	    add_target(L, Jump, St);
	{fcmp, [Type, Src1, Src2]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_fcmp(Compile, Type, Src1, Src2),
	    add_target(L, Jump, St);
	{jump, [Type]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_jump(Compile, Type),
	    add_target(L, Jump, St);
	{ijump,[Type, Src]} ->
	    ok = sljit:emit_ijump(Compile, Type, Src),
	    St;
	{mjump,[Type, Mod, Fun]} -> 
	    ok = sljit:emit_mjump(Compile, Type, Mod, Fun),
	    St;
	{call, [Type, ArgTypes]} ->
	    L = maps:get(label_name, St),
	    Jump = sljit:emit_call(Compile, Type, ArgTypes),
	    add_target(L, Jump, St);
	{icall, [Type,ArgTypes,Src]} ->
	    ok = sljit:emit_icall(Compile, Type, ArgTypes, Src),
	    St;
	%% like ICALL but with mod:fun instead
	{mcall, [Type,ArgTypes,Mod,Fun]} ->
	    ok = sljit:emit_mcall(Compile, Type, ArgTypes, Mod, Fun),
	    St;
	{enter,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    ok = sljit:emit_enter(Compile,Options,ArgTypes,Scratches,Saved,
				  LocalSize),
	    St;
	{set_context,[Options,ArgTypes,Scratches,Saved,LocalSize]} ->
	    ok = sljit:set_context(Compile,Options,ArgTypes,Scratches,Saved,
				   LocalSize),
	    St;
	{return_void,[]} ->
	    ok = sljit:emit_return_void(Compile),
	    St;
	{return,[Op,Src]} ->
	    ok = sljit:emit_return(Compile, Op, Src),
	    St;
	{simd_op2,[Op,Dst,Src1,Src2]} ->
	    ok = sljit:emit_simd_op2(Compile,Op,Dst,Src1,Src2),
	    St;
	{simd_mov,[Op,VReg,SrcDst]} ->
	    ok = sljit:emit_simd_mov(Compile,Op,VReg,SrcDst),
	    St;
	{const_name, [Name]} ->  %% synthetic
	    St#{const_name => Name};
	{const, [Op,Dst,InitValue]} -> 
	    Name = maps:get(const_name, St),
	    Const = sljit:emit_const(Compile, Op, Dst, InitValue),
	    ok = sljit:constant(Compile, Name, Const),
	    CList = maps:get(constants, St, []),
	    St#{ constants => [{Name, Const} | CList], 
		 const_name => undefined };
	{op_addr, [Op, Dst]} ->
	    Jump = sljit:emit_op_addr(Compile, Op, Dst),
	    St1 = case maps:get(label_name, St, undefined) of
		      undefined -> St;
		      L0 ->
			  add_target(L0, Jump, St)
		  end,
	    case maps:get(jump_name, St1, undefined) of
		undefined ->
		    St1;
		Jsrc ->
		    noemit(Compile, jump_addr, [Jsrc, Jump]),
		    maps:remove(jump_name, St1)
	    end;
	{Fmt,_Args} ->
	    throw({error, {unknown_format, Fmt}})
    end.

asm_ins_list(Compile, [Ins|Instructions], St) ->
    St1 = asm_ins(Compile, Ins, St),
    asm_ins_list(Compile, Instructions, St1);
asm_ins_list(_, [], St) ->
    St.

asm_ins(Compile, Instruction, St) ->
    Fmt = fmt(Instruction),
    ?dbg("ASM (fmt=~w) ~p, st=~p~n", [Fmt, Instruction,St]),
    case Fmt of
	module ->
	    {module, M} = Instruction,
	    ok = emit(Compile, module, [M]),
	    St#{module => M};
	function ->
	    {function, F} = Instruction,
	    Label = emit(Compile, function, [F]),
	    St1 = ins_func(Compile, F, Label, St),
	    St1#{function => F};
	op0 -> ins_op0(Compile, Instruction, St);
	op1 -> ins_op1(Compile, Instruction, St);
	op2 -> ins_op2(Compile, Instruction, St);
	%% op2u -> ins_op2u(Compile, Instruction, St);
	op2r -> ins_op2(Compile, Instruction, St);
	%% shift_into -> ins_shift_info(Compile, Instruction, St);
	op_src -> ins_op_src(Compile, Instruction, St);
	op_dst -> ins_op_dst(Compile, Instruction, St);
	fop1 -> ins_fop1(Compile, Instruction, St);
	fop2 -> ins_fop2(Compile, Instruction, St);
	%%fop2r -> ins_fop2r(Compile, Instruction, St);
	fset32 -> ins_fset32(Compile, Instruction, St);
	fset64 -> ins_fset64(Compile, Instruction, St);
	%%fcopy -> ins_fcopy(Compile, Instruction, St);
	label -> ins_label(Compile, Instruction, St);
	jump -> ins_jump(Compile, Instruction, St);
	call -> ins_call(Compile, Instruction, St);
	%% cmp -> ins_cmp(Compile, Instruction, St);
	%% fcmp -> ins_fcmp(Compile, Instruction, St);
	ijump -> ins_ijump(Compile, Instruction, St);
	icall -> ins_icall(Compile, Instruction, St);
	enter -> ins_enter(Compile, Instruction, St);
	set_context -> ins_enter(Compile, Instruction, St);
	return -> ins_return(Compile, Instruction, St);
	simd_op2 -> ins_simd_op2(Compile, Instruction, St);
	simd_mov -> ins_simd_mov(Compile, Instruction, St);
	const -> ins_const(Compile, Instruction, St);
	op_addr -> ins_op_addr(Compile,  Instruction, St);
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

-undef(OP_SRC_NAME).
-define(OP_SRC_NAME(I, N), I -> N; ).

ins_op_src(Compile, {Ins, S}, St) ->
    Op = case Ins of
	     ?OP_SRC_LIST
	     _ -> throw({error, {unknown_op_src, Ins}})
	 end,
    Src = src(S),
    ok = emit(Compile, op_src, [Op, Src]),
    St.

-undef(OP_DST_NAME).
-define(OP_DST_NAME(I, N), I -> N; ).

ins_op_dst(Compile, {Ins, D}, St) ->
    Op = case Ins of
	     ?OP_DST_LIST
	     _ -> throw({error, {unknown_op_dst, Ins}})
	 end,
    Dst = dst(D),
    ok = emit(Compile, op_dst, [Op, Dst]),
    St.


-undef(OP1_NAME).
-define(OP1_NAME(I, N), I -> N; ).

ins_op1(Compile, {Ins, D, S}, St) ->
    Op = case Ins of
	     ?OP1_LIST
	     _ -> throw({error, {unknown_op1, Ins}})
	 end,
    Dst = dst(D),
    Src = src(S),
    ok = emit(Compile, op1, [Op, Dst, Src]),
    St.

-undef(OP2_NAME).
-define(OP2_NAME(I, N), I -> N; ).

-undef(OP2R_NAME).
-define(OP2R_NAME(I, N), I -> N; ).

-undef(OP_SHIFT_INTO_NAME).
-define(OP_SHIFT_INTO_NAME(I, N), I -> (N); ).

ins_op2(Compile, {Op, S1, S2}, St) ->
    ins_op2(Compile, {Op, [], S1, S2}, St);

ins_op2(Compile, {Op, Fs, S1, S2}, St) when is_list(Fs) ->
    OpC = case Op of
	     ?OP2_LIST
	     _ -> throw({error, {unknown_op2, Op}})
	 end,
    OpF = ins_set_flags(Fs),
    Src1 = src(S1),
    Src2 = src(S2),
    ok = emit(Compile, op2u, [OpC bor OpF, Src1, Src2]),
    St;
ins_op2(Compile, {Op, D, S1, S2}, St) ->
    ins_op2(Compile, {Op, [], D, S1, S2}, St);
ins_op2(Compile, {Op, Fs, D, S1, S2}, St) when is_list(Fs) ->
    OpC = case Op of
	      ?OP2_LIST
	      ?OP2R_LIST
	      _ -> throw({error, {unknown_op2, Op}})
	  end,
    OpF = ins_set_flags(Fs),
    Src1 = src(S1),
    Src2 = src(S2),
    Dst = dst(D),
    OPR = case Op of 
	      ?OP2R_LIST
	      _ -> false
	  end,
    if OPR =:= false ->
	    ok = emit(Compile, op2,
		      [OpC bor OpF, Dst, Src1, Src2]);
       true ->
	    ok = emit(Compile, op2r,
		      [OpC bor OpF, Dst, Src1, Src2])
    end,
    St;
ins_op2(Compile, {Op, D, S1, S2, S3}, St) ->
    ins_op2(Compile, {Op, [], D, S1, S2, S3}, St);
ins_op2(Compile, {Op, Fs, D, S1, S2, S3}, St) ->
    OpC = case Op of
	      ?OP_SHIFT_INTO_LIST
	      _ -> throw({error, {unknown_shift_into_op, Op}})
	 end,
    OpC1 = case Fs of
	       [] -> OpC;
	       [nz] ->  OpC bor ?SLJIT_SHIFT_INTO_NON_ZERO
	   end,
    Dst = reg(D),
    Src1 = reg(S1),
    Src2 = reg(S2),
    Src3 = src(S3),
    ok = emit(Compile, shift_into, [OpC1, Dst, Src1, Src2, Src3]),
    St.

-undef(FOP1_NAME).
-define(FOP1_NAME(I, N), I -> N; ).

ins_fop1(Compile, {Ins, D, S}, St) ->
    Op = case Ins of
	     ?FOP1_LIST
	     _ -> throw({error, {unknown_fop1, Ins}})
	 end,
    Dst = fdst(D),
    Src = fsrc(S),
    %% fixme: emit imm float ?
    ok = emit(Compile, fop1, [Op, Dst, Src]),
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
	      [f_equal] -> Op bor ?SLJIT_SET_F_EQUAL;
	      [f_not_equal] -> Op bor ?SLJIT_SET_F_NOT_EQUAL;
	      [f_less] -> Op bor ?SLJIT_SET_F_LESS;
	      [f_greater_equal] -> Op bor ?SLJIT_SET_F_GREATER_EQUAL;
	      [f_greater] -> Op bor ?SLJIT_SET_F_GREATER;
	      [f_less_equal] -> Op bor ?SLJIT_SET_F_LESS_EQUAL;
	      [] -> Op
	  end,
    Src1 = fsrc(S1),
    Src2 = fsrc(S2),
    Dst = fdst(D),
    case Dst of
	{Dr={fr,_}, 0} ->
	    ok = emit(Compile, fop2r, 
		      [Op1, Dr, Src1, Src2]);
	{Dr={fs,_}, 0} ->
	    ok = emit(Compile, fop2r, 
		      [Op1, Dr, Src1, Src2]);
	_ ->
	    emit(Compile, fop2, 
		 [Op1, Dst, Src1, Src2]),
	    St
    end.


ins_set_flags(Fs) ->
    ins_set_flags_(Fs,0,0).

ins_set_flags_([],Z,Var) ->
    Z bor Var;
ins_set_flags_([F|Fs],Z,Var) ->
    Set = set_flag(F),
    if Set =:= ?SLJIT_SET_Z ->
	    ins_set_flags_(Fs,Set,Var);
       Var =:= Set ->
	    ins_set_flags_(Fs,Z,Var);
       Var =:= 0 ->
	    ins_set_flags_(Fs,Z,Set);
       true ->
	    throw({error, {only_one_var_flag, F}})
    end.

set_flag(F) ->
    case F of
	less -> ?SLJIT_SET_LESS;
	greater_equal -> ?SLJIT_SET_GREATER_EQUAL;
	greater -> ?SLJIT_SET_GREATER;
	less_equal -> ?SLJIT_SET_LESS_EQUAL;
	sig_less -> ?SLJIT_SET_SIG_LESS;
	sig_greater_equal -> ?SLJIT_SET_SIG_GREATER_EQUAL;
	sig_greater -> ?SLJIT_SET_SIG_GREATER;
	sig_less_equal -> ?SLJIT_SET_SIG_LESS_EQUAL;
	equal -> ?SLJIT_SET_Z;
	not_equal -> ?SLJIT_SET_Z;
	zero -> ?SLJIT_SET_Z;
	z -> ?SLJIT_SET_Z;
	carry -> ?SLJIT_SET_CARRY;
	overflow -> ?SLJIT_SET_OVERFLOW;
	%% float
	f_equal -> ?SLJIT_SET_F_EQUAL;
	f_not_equal -> ?SLJIT_SET_F_NOT_EQUAL;
	f_less -> ?SLJIT_SET_F_LESS;
	f_greater_equal -> ?SLJIT_SET_F_GREATER_EQUAL;
	f_greater -> ?SLJIT_SET_F_GREATER;
	f_less_equal -> ?SLJIT_SET_F_LESS_EQUAL;
	
	_ when is_integer(F) -> F
    end.

-spec cmp(sljit:compiler(), {cmp, Test::atom(), S1::imm()|reg(), S2::imm()|reg()}, St::map()) -> sljit:jump().
cmp(Compile, {cmp, Test, S1, S2}, _St) ->
    Type = enc_cmp(Test),
    Src1 = src(S1),
    Src2 = src(S2),
    emit(Compile, cmp, [Type, Src1, Src2]).

enc_cmp(Test) ->
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
	%% float
	f_equal -> ?SLJIT_F_EQUAL;
	f_not_equal -> ?SLJIT_F_NOT_EQUAL;
	f_less -> ?SLJIT_F_LESS;
	f_greater_equal -> ?SLJIT_F_GREATER_EQUAL;
	f_greater -> ?SLJIT_F_GREATER;
	f_less_equal -> ?SLJIT_F_LESS_EQUAL;
	_ when is_integer(Test) -> Test
    end.

dec_cmp(Test) ->
    case Test band 16#ff of
	%% unsigned compare
	?SLJIT_EQUAL -> equal;
	?SLJIT_NOT_EQUAL -> not_equal;
	?SLJIT_LESS -> less;
	?SLJIT_GREATER_EQUAL -> greater_equal;
	?SLJIT_GREATER -> greater;
	?SLJIT_LESS_EQUAL -> less_equal;
	%% signed compare
	?SLJIT_SIG_LESS -> sig_less;
	?SLJIT_SIG_GREATER_EQUAL -> sig_greater_equal;
	?SLJIT_SIG_GREATER -> sig_greater;
	?SLJIT_SIG_LESS_EQUAL -> sig_less_equal;
	%% float
	?SLJIT_F_EQUAL -> f_equal;
	?SLJIT_F_NOT_EQUAL -> f_not_equal;
	?SLJIT_F_LESS -> f_less;
	?SLJIT_F_GREATER_EQUAL -> f_greater_equal;
	?SLJIT_F_GREATER -> f_greater;
	?SLJIT_F_LESS_EQUAL -> f_less_equal;
	_  -> Test
    end.

enc_status(Test) ->
    case Test of
	always -> ?SLJIT_JUMP;
	%% compare
	equal -> ?SLJIT_EQUAL;
	not_equal -> ?SLJIT_NOT_EQUAL;
	less -> ?SLJIT_LESS;
	less_equal -> ?SLJIT_LESS_EQUAL;
	greater -> ?SLJIT_GREATER;
	greater_equal -> ?SLJIT_GREATER_EQUAL;
	%% signed compare
	sig_less -> ?SLJIT_SIG_LESS;
	sig_greater_equal -> ?SLJIT_SIG_GREATER_EQUAL;
	sig_greater -> ?SLJIT_SIG_GREATER;
	sig_less_equal -> ?SLJIT_SIG_LESS_EQUAL;
	%% float
	f_equal -> ?SLJIT_F_EQUAL;
	f_not_equal -> ?SLJIT_F_NOT_EQUAL;
	f_less -> ?SLJIT_F_LESS;
	f_greater_equal -> ?SLJIT_F_GREATER_EQUAL;
	f_greater -> ?SLJIT_F_GREATER;
	f_less_equal -> ?SLJIT_F_LESS_EQUAL;
	%% flags
	overflow -> ?SLJIT_OVERFLOW;
	not_overflow -> ?SLJIT_NOT_OVERFLOW;
	carry -> ?SLJIT_CARRY;
	not_carry -> ?SLJIT_NOT_CARRY;
	_ when is_integer(Test) -> Test
    end.

dec_status(Test) ->
    case Test band 16#ff of  %% fixme mask, how many bits?
	?SLJIT_JUMP -> always;
	?SLJIT_EQUAL -> equal;
	?SLJIT_NOT_EQUAL -> not_equal;
	?SLJIT_LESS -> less;
	?SLJIT_LESS_EQUAL -> less_equal;
	?SLJIT_GREATER -> greater;
	?SLJIT_GREATER_EQUAL -> greater_equal;
	?SLJIT_OVERFLOW -> overflow;
	?SLJIT_NOT_OVERFLOW -> not_overflow;
	?SLJIT_CARRY -> carry;
	?SLJIT_NOT_CARRY -> not_carry;
	_ -> Test
    end.

dec_jump_src(Type, St) when Type band ?SLJIT_REWRITABLE_JUMP =:= 
			    ?SLJIT_REWRITABLE_JUMP ->
    case maps:find(jump_name,St) of
	{ok,Name} ->
	    {[{from,Name}], maps:remove(jump_name, St)};
	error ->
	    {[], St}
    end;
dec_jump_src(_Type, St) ->
    {[], St}.

    

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
		    noemit(Compile, label_addr, [L, Label]),
		    lists:foreach(fun(Jump) -> 
					  %% code generation?
					  sljit:set_label(Jump, Label)
				  end, Jumps),
		    St#{ {label, L} => Label, {target, L} => [] };
		error ->
		    Label = emit(Compile, label, []),
		    noemit(Compile, label_addr, [L, Label]),
		    St#{ {label, L} => Label }
	    end
    end.

ins_func(Compile, F, Label, St) ->
    case maps:find({label, F}, St) of
	{ok, _Label} ->
	    throw({error, {duplicate_label, F}});
	error ->
	    case maps:find({target, F}, St) of
		{ok, Jumps} ->
		    noemit(Compile, label_addr, [F, Label]),
		    lists:foreach(fun(Jump) -> 
					  %% code generation?
					  sljit:set_label(Jump, Label)
				  end, Jumps),
		    St#{ {label, F} => Label, {target, F} => [] };
		error ->
		    noemit(Compile, label_addr, [F, Label]),
		    St#{ {label, F} => Label }
	    end
    end.

ins_jump(Compile, {jump, {Mod,Fun}}, St) when is_atom(Mod), is_atom(Fun) ->
    %% fixme conditional/rewriteable mod:fun entries?
    ok = emit(Compile, mjump, [?SLJIT_JUMP, Mod, Fun]),
    St;
ins_jump(Compile, {jump, L0}, St) ->
    make_jump(Compile, [always], L0, St);
ins_jump(Compile, {jump, Options, L0},St) when is_list(Options) ->
    make_jump(Compile, Options, L0, St).

make_jump(Compile,Options,LabelName,St) ->
    ok = emit(Compile, label_name, [LabelName]),
    Jump = make_jump_(Compile, Options, undefined, ?SLJIT_JUMP),
    add_target(LabelName, Jump, St).

%% process jump options
make_jump_(Compile, [], JSrc, {T, Src1, Src2}) ->
    set_jump_name(Compile, JSrc),
    if ?compare_float_type(T) ->
	    Jump = emit(Compile, fcmp, [dynamic_jump(JSrc, T),Src1,Src2]),
	    set_jump_src(Compile,JSrc,Jump);
       true ->
	    Jump = emit(Compile, cmp, [dynamic_jump(JSrc, T),Src1,Src2]),
	    set_jump_src(Compile,JSrc,Jump)
    end;
make_jump_(Compile, [], JSrc, T) when is_integer(T) ->
    set_jump_name(Compile, JSrc),
    Jump = emit(Compile, jump, [dynamic_jump(JSrc, T)]),
    set_jump_src(Compile,JSrc,Jump);

make_jump_(Compile, [Option|Options], JSrc, Condition) ->
    case Option of
	{from, JSrc1} when is_atom(JSrc1) ->
	    make_jump_(Compile, Options, JSrc1, Condition);
	{Test, S1, S2} ->
	    T = enc_cmp(Test), 
	    if ?compare_float_type(T) ->
		    make_jump_(Compile, Options, JSrc, {T,fsrc(S1),fsrc(S2)});
	       true ->
		    make_jump_(Compile, Options, JSrc, {T,src(S1),src(S2)})
	    end;
	Test when is_atom(Test); is_integer(Test)  ->
	    T = enc_status(Test),
	    make_jump_(Compile, Options, JSrc, T)
    end.

ins_ijump(Compile, {ijump, Options}, St) ->
    make_ijump_(Compile, Options, ?SLJIT_JUMP, undefined, undefined, 
		undefined, undefined, St).

make_ijump_(Compile, [], Type, _Src, Dst, Def, JSrc, St) ->
    if JSrc =:= undefined -> ok;
       true ->
	    ok = emit(Compile, const_name, [JSrc])
    end,
    Const = emit(Compile, const, [?SLJIT_MOV, Dst, Def]),
    noemit(Compile, constant, [JSrc, Const]), %% jsrc=undefined => anonymous
    ok = emit(Compile, ijump, [Type, Dst]),
    St;
make_ijump_(Compile, [], Type, {Src}, _Dst, _Def, _JSrc, St) ->
    ok = emit(Compile, ijump, [Type, Src]),
    St;

make_ijump_(Compile, [Option|Options], T, Src, Dst, Def, JSrc, St) ->
    case Option of
	{src,S} -> %% jump indirect from source register/mem
	    make_ijump_(Compile, Options, T, src(S), Dst, Def, JSrc, St);
	{dst,D} -> %% jump indirect via destination register/mem,
	    make_ijump_(Compile, Options, T,  Src, dst(D), Def, JSrc, St);
	{default,L} when is_atom(L) -> %% default destination (dst)
	    make_ijump_(Compile, Options, T, Src, Dst,  L, JSrc, St);
	{from,JSrc1} when is_atom(JSrc1) -> %% jump source
	    make_ijump_(Compile, Options, T, Src, Dst,  Def, JSrc1, St);
	fast_call ->
	    make_ijump_(Compile, Options, ?SLJIT_FAST_CALL,
			Src, Dst, Def, JSrc, St);
	jump ->
	    make_ijump_(Compile, Options, ?SLJIT_JUMP,
			Src, Dst, Def, JSrc, St);
	Type when is_integer(Type) ->
	    make_ijump_(Compile, Options, Type,
			Src, Dst, Def, JSrc, St)
    end.

decode_ijump_type(?SLJIT_JUMP) -> jump;
decode_ijump_type(?SLJIT_FAST_CALL) -> fast_call;
decode_ijump_type(T) -> T.
    

dynamic_jump(undefined, Type) ->  Type;
dynamic_jump(_JSrc, Type) -> Type bor ?SLJIT_REWRITABLE_JUMP.

%% save jump source, to be used with set_jump(JumpSrc, LabelName)
%% at runtime.
set_jump_name(_Compile,undefined) ->
    ok;
set_jump_name(Compile,JumpSrc) ->
    ok = emit(Compile, jump_name, [JumpSrc]),
    ok.

set_jump_src(_Compile,undefined, Jump) ->
    Jump;
set_jump_src(Compile,JumpSrc, Jump) ->
    noemit(Compile, jump_addr, [JumpSrc, Jump]),
    Jump.

ins_op_addr(Compile, {op_addr, JSrc, OpName, D, L}, St) ->
    ok = emit(Compile, label_name, [L]),
    set_jump_name(Compile, JSrc),
    Dst = dst(D),
    Op = case OpName of
	     mov_addr -> ?SLJIT_MOV_ADDR;
	     mov_abs_addr -> ?SLJIT_MOV_ABS_ADDR;
	     add_abs_addr -> ?SLJIT_ADD_ABS_ADDR
	 end,
    Jump = emit(Compile, op_addr, [Op, Dst]),
    set_jump_src(Compile,JSrc,Jump),
    add_target(L, Jump, St).

ins_const(Compile, {const, Name, Type, D, InitValue}, St) ->
    Op = case Type of
	     mov -> ?SLJIT_MOV;
	     mov32 -> ?SLJIT_MOV32;
	     mov_s32 -> ?SLJIT_MOV_S32;
	     mov_u8 -> ?SLJIT_MOV_U8;
	     mov32_u8 -> ?SLJIT_MOV32_U8;
	     _ when is_integer(Type) -> Type
	 end,
    Dst = dst(D),
    ok = emit(Compile, const_name, [Name]),
    Const = emit(Compile, const, [Op, Dst, InitValue]),
    ok = noemit(Compile, constant, [Name, Const]),
    CList = maps:get(constants, St, []),
    St#{ constants => [{Name, Const} | CList] }.

ins_call(Compile, {call, Type, Ret, Args, {Mod,Fun}}, St) ->
    Type1 = encode_icall_type(Type),  %% uses icall !!
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    ok = emit(Compile, mcall, [Type1, ArgTypes, Mod, Fun]),
    St;
ins_call(Compile, {call, Type, Ret, Args, L0}, St) ->
    ok = emit(Compile, label_name, [L0]),
    Type1 = encode_call_type(Type),
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    Jump = emit(Compile, call, [Type1, ArgTypes]),
    %% Jump = sljit:emit_call(Compile, Type1, ArgTypes),
    add_target(L0, Jump, St).

ins_icall(Compile, {icall, Type, Ret, Args, S}, St) ->
    Src = src(S),
    Type1 = encode_icall_type(Type),
    RetType = encode_ret(Ret),
    ArgTypes = RetType bor encode_args(Args),
    ok = emit(Compile, icall, [Type1, ArgTypes, Src]),
    St.

ins_enter(Compile, {enter, Options0, RetType0, ArgTypes0, 
		    Scratches0, Saved0, LocalSize}, St) ->
    Options = encode_enter_options(Options0),
    Scratches = encode_enter_regs(Scratches0),
    Saved = encode_enter_regs(Saved0),
    RetType = encode_ret(RetType0),
    ArgTypes = RetType bor encode_args(ArgTypes0),
    ok = emit(Compile, enter,
	      [Options, ArgTypes, Scratches, Saved, LocalSize]),
    St;
ins_enter(Compile, {set_context, Options0, RetType0, ArgTypes0, 
		    Scratches0, Saved0, LocalSize}, St) ->
    Options = encode_enter_options(Options0),
    Scratches = encode_enter_regs(Scratches0),
    Saved = encode_enter_regs(Saved0),
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
	    Src = src(S),
	    ok = emit(Compile, return, [?SLJIT_MOV, Src]);
	mov_p ->
	    Src = src(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_P, Src]);
	mov_f32 ->
	    %% fixme: emit imm float
	    Src = fsrc(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_F32, Src]);
	mov_f64 ->
	    %% fixme: emit imm float
	    Src = fsrc(S),
	    ok = emit(Compile, return, [?SLJIT_MOV_F64, Src]);
	_ when is_integer(Op0) ->
	    Src = src(S), 
	    ok = emit(Compile, return, [Op0, Src])
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
    OpF = encode_simd_op2_flags(Fs),
    Dst = vreg(D),
    Src1 = vreg(S1),
    Src2 = vsrc(S2),
    ok = emit(Compile, simd_op2, [Op bor OpF, Dst, Src1, Src2]),
    St.


encode_simd_op2_flags(Fs) ->
    vflags(Fs, 0,
	   ?SLJIT_SIMD_REG_128,
	   ?SLJIT_SIMD_ELEM_8,
	   ?SLJIT_SIMD_MEM_UNALIGNED).

vflags([],F,R,E,M) ->
    F bor R bor E bor M;
vflags([Flag|Fs],F,R,E,M) ->
    case Flag of
	float -> vflags(Fs, F bor ?SLJIT_SIMD_FLOAT, R, E, M);
	test -> vflags(Fs, F bor ?SLJIT_SIMD_TEST, R, E, M);

	reg_64 -> vflags(Fs, F, ?SLJIT_SIMD_REG_64, E, M);
	reg_128 -> vflags(Fs, F, ?SLJIT_SIMD_REG_128, E, M);
	reg_256 -> vflags(Fs, F, ?SLJIT_SIMD_REG_256, E, M);
	reg_512 -> vflags(Fs, F, ?SLJIT_SIMD_REG_512, E, M);

	elem_8 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_8, M);
	elem_16 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_16, M);
	elem_32 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_32, M);
	elem_64 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_64, M);
	elem_128 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_128, M);
	elem_256 -> vflags(Fs, F, R, ?SLJIT_SIMD_ELEM_256, M);

	unaligned -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_UNALIGNED);
	aligned_16 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_16);
	aligned_32 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_32);
	aligned_64 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_64);
	aligned_128 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_128);
	aligned_256 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_256);
	aligned_512 -> vflags(Fs, F, R, E, ?SLJIT_SIMD_MEM_ALIGNED_512)
    end.

%% <<Mem:6, Elem:6, Reg:6, Type:12>>
decode_simd_mov_flags(Flags) ->
    decode_simd_op2_flags(Flags).
decode_simd_op2_flags(Flags) ->
    Reg  = 
	case Flags band (16#3f  bsl 12) of
	    ?SLJIT_SIMD_REG_64 -> reg_64;
	    ?SLJIT_SIMD_REG_128 -> reg_128;
	    ?SLJIT_SIMD_REG_256 -> reg_256;
	    ?SLJIT_SIMD_REG_512 -> reg_512
	end,
    Elem = case Flags band (16#3f  bsl 18) of
	       ?SLJIT_SIMD_ELEM_8 -> elem_8;
	       ?SLJIT_SIMD_ELEM_16 -> elem_16;
	       ?SLJIT_SIMD_ELEM_32 -> elem_32;
	       ?SLJIT_SIMD_ELEM_64 -> elem_64;
	       ?SLJIT_SIMD_ELEM_128 -> elem_128;
	       ?SLJIT_SIMD_ELEM_256 -> elem_256
	   end,
    Mem  = case Flags band (16#3f  bsl 24) of
	       ?SLJIT_SIMD_MEM_UNALIGNED -> unaligned;
	       ?SLJIT_SIMD_MEM_ALIGNED_16 -> aligned_16;
	       ?SLJIT_SIMD_MEM_ALIGNED_32 -> aligned_32;
	       ?SLJIT_SIMD_MEM_ALIGNED_64 -> aligned_64;
	       ?SLJIT_SIMD_MEM_ALIGNED_128 -> aligned_128;
	       ?SLJIT_SIMD_MEM_ALIGNED_256 -> aligned_256;
	       ?SLJIT_SIMD_MEM_ALIGNED_512 -> aligned_512
	   end,
    if Flags band ?SLJIT_SIMD_FLOAT =/= 0 ->
	    [float];
       true ->
	    []
    end ++
    if Flags band ?SLJIT_SIMD_TEST =/= 0 ->
	    [test];
       true ->
	    []
    end ++ [Reg,Elem,Mem].

-undef(SIMD_MOV_NAME).
-define(SIMD_MOV_NAME(I, N), I -> N; ).

ins_simd_mov(Compile, {Ins, VReg, SrcDst}, St) ->
    ins_simd_mov(Compile, {Ins, [], VReg, SrcDst}, St);
ins_simd_mov(Compile, {Ins, Fs, VReg, SrcDst}, St) when is_list(Fs) ->
    Op = case Ins of
	     ?SIMD_MOV_LIST
	     _ -> throw({error, {unknown_simd_op2, Ins}})
	 end,
    OpF = encode_simd_op2_flags(Fs),
    Reg = vreg(VReg),
    Src = src(SrcDst),
    ok = emit(Compile, simd_mov, [Op bor OpF, Reg, Src]),
    St.


ins_fset32(Compile, {fset32, Reg, Value}, St) ->
    ok = emit(Compile, fset32, [freg(Reg), Value]),
    St.

ins_fset64(Compile, {fset64, Reg, Value}, St) ->
    ok = emit(Compile, fset64, [freg(Reg), Value]),
    St.

%% compile and emit binary 
emit({fd,Fd,Compile}, Fmt, Args) ->
    ?dbg("EMIT ~s ~p\n", [Fmt, Args]),
    SLFunc = emit_func(Fmt),
    Res = apply(sljit, SLFunc, [Compile|Args]),
    ok = emitf(Fd, Fmt, Args),
    Res;
emit(Compile, Fmt, Args) ->
    ?dbg("EMIT ~s ~p\n", [Fmt, Args]),
    SLFunc = emit_func(Fmt),
    apply(sljit, SLFunc, [Compile|Args]).

%% encode and write Fmt Args to file
emitf(Fd, Fmt, Args) when is_atom(Fmt) ->
    Bin = term_to_binary({Fmt,Args}),
    true = (byte_size(Bin) < 256),
    Bin1 = <<(byte_size(Bin)),Bin/binary>>,
    file:write(Fd, Bin1).

%% compile and emit binary 
noemit({fd,_Fd,Compile}, Fmt, Args) ->
    SLFunc = emit_func(Fmt),
    apply(sljit, SLFunc, [Compile|Args]);
noemit(Compile, Fmt, Args) ->
    SLFunc = emit_func(Fmt),
    apply(sljit, SLFunc, [Compile|Args]).

%% Convert format name into emit function name
emit_func(op0) -> emit_op0;
emit_func(op1) -> emit_op1;
emit_func(op2) -> emit_op2;
emit_func(op2u) -> emit_op2u;
emit_func(op2r) -> emit_op2r;
emit_func(op_src) -> emit_op_src;
emit_func(op_dst) -> emit_op_dst;
emit_func(shift_into) -> emit_shift_into;
emit_func(fop1) -> emit_fop1;
emit_func(fop2) -> emit_fop2;
emit_func(fop2r) -> emit_fop2r;
emit_func(fset32) -> emit_fset32;
emit_func(fset64) -> emit_fset64;
emit_func(fcopy) -> emit_fcopy;
emit_func(label) -> emit_label;
emit_func(jump) -> emit_jump;
emit_func(constant) -> constant;
emit_func(ijump) -> emit_ijump;
emit_func(cmp) -> emit_cmp;
emit_func(fcmp) -> emit_fcmp;
emit_func(enter) -> emit_enter;
emit_func(call) -> emit_call;
emit_func(icall) -> emit_icall;
emit_func(mcall) -> emit_mcall;
emit_func(mjump) -> emit_mjump;
emit_func(set_context) -> set_context;
emit_func(return_void) -> emit_return_void;
emit_func(return) -> emit_return;
emit_func(simd_op2) -> emit_simd_op2;
emit_func(simd_mov) -> emit_simd_mov;
emit_func(label_name) -> label_name;
emit_func(jump_name) -> jump_name;
emit_func(const) -> emit_const;
emit_func(const_name) -> const_name;
emit_func(module) -> module;
emit_func(function) -> function;
emit_func(op_addr) -> emit_op_addr;
emit_func(jump_addr) -> jump_addr;
emit_func(label_addr) -> label_addr.


-undef(OP0_NAME).
-define(OP0_NAME(I, N), I -> op0; ).

-undef(OP_SRC_NAME).
-define(OP_SRC_NAME(I, N), I -> op_src; ).

-undef(OP_DST_NAME).
-define(OP_DST_NAME(I, N), I -> op_dst; ).

-undef(OP1_NAME).
-define(OP1_NAME(I, N), I -> op1; ).

-undef(OP2_NAME).
-define(OP2_NAME(I, N), I -> op2; ).

-undef(OP2R_NAME).
-define(OP2R_NAME(I, N), I -> op2r; ).

-undef(FOP1_NAME).
-define(FOP1_NAME(I, N), I -> fop1; ).

-undef(FOP2_NAME).
-define(FOP2_NAME(I, N), I -> fop2; ).

-undef(FOP2R_NAME).
-define(FOP2R_NAME(I, N), I -> fop2r; ).

-undef(OP_FCOPY_NAME).
-define(OP_FCOPY_NAME(I, N), I -> fcopy; ).

-undef(OP_SHIFT_INTO_NAME).
-define(OP_SHIFT_INTO_NAME(I, N), I -> shift_into; ).

-undef(SIMD_OP2_NAME).
-define(SIMD_OP2_NAME(I, N), I -> simd_op2; ).

-undef(SIMD_MOV_NAME).
-define(SIMD_MOV_NAME(I, N), I -> simd_mov; ).

fmt(Ins) ->
    case opcode(Ins) of
	?OP0_LIST
	?OP_SRC_LIST
	?OP_DST_LIST
	?OP1_LIST
	?OP2_LIST
	?OP2R_LIST
	?FOP1_LIST
	?FOP2_LIST
	?OP_FCOPY_LIST
	?SIMD_OP2_LIST
	?SIMD_MOV_LIST
	%% ?OP_SHIFT_INTO_LIST - handled by op2
	label -> label;
	jump -> jump;
	enter -> enter;
	set_context -> set_context;
	return -> return;
	return_void -> return_void;
	label_name -> label_name;
	const_name -> const_name;
	jump_name -> jump_name;
	module -> module;
	function -> function;
	fset32 -> fset32;
	fset64 -> fset64;
	call -> call;
	icall -> icall;
	ijump -> ijump;
	const -> const;
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

opcode(T) when is_atom(T) -> T;
opcode(T) when is_atom(element(1, T)) ->  element(1, T).


%% generate destination
sd({mem,Imm},_) when is_integer(Imm) ->       {mem, Imm};
sd({mem,R1},_) ->                             {mem,reg(R1)};
sd({mem,R1,Imm},_) when is_integer(Imm) ->    {mem,reg(R1),Imm};
sd({mem,R1,R2},_) ->                          {mem,reg(R1),reg(R2)};
sd({mem,R1,R2,Imm},_) when is_integer(Imm) -> {mem,reg(R1),reg(R2),Imm};
sd({r,I},r) when is_integer(I), I>=0, I=<127 -> {r,I};
sd({s,I},r) when is_integer(I), I>=0, I=<127 -> {s,I};
sd(R, r) when is_atom(R) -> reg(R);
sd({fr,I},f) when is_integer(I), I>=0, I=<127 -> {fr,I};
sd({fs,I},f) when is_integer(I), I>=0, I=<127 -> {fs,I};
sd(R, f) when is_atom(R) -> freg(R);
sd({vr,I},v) when is_integer(I), I>=0, I=<127 -> {vr,I};
sd({vs,I},v) when is_integer(I), I>=0, I=<127 -> {vs,I};
sd(R, v) when is_atom(R) -> vreg(R).


-spec dst(dst()) -> dst().
dst(D) -> sd(D, r).

-spec src(src()) -> src().
src(Imm) when is_integer(Imm) ->       {imm, Imm};
src({imm,Imm}) when is_integer(Imm) -> {imm, Imm};
src(S) -> sd(S, r).

-spec fdst(fdst()) -> fdst().
fdst(D) -> sd(D, f).

-spec fsrc(fsrc()) -> fsrc().
fsrc(S) -> sd(S, f).

-spec vdst(vdst()) -> vdst().
vdst(D) -> sd(D, v).

-spec vsrc(vsrc()) -> vsrc().
vsrc(S) -> sd(S, v).



encode_enter_options([]) -> 0;
encode_enter_options([{keep,N}|Opts]) when N >= 1, N =< 3 ->
    ?SLJIT_ENTER_KEEP(N) bor encode_enter_options(Opts);
encode_enter_options([reg_arg |Opts]) ->
    ?SLJIT_ENTER_REG_ARG bor encode_enter_options(Opts).

decode_enter_options(Type) ->
    case Type band 3 of
	0 -> [];
	1 -> [{keep,1}];
	2 -> [{keep,2}];
	3 -> [{keep,3}]
    end ++ 
	if ?test_flag(?SLJIT_ENTER_REG_ARG,Type) ->
		[reg_arg];
	   true ->
		[]
	end.

encode_enter_regs([]) -> 0;     
encode_enter_regs([{reg,R}|Regs]) ->
    R bor encode_enter_regs(Regs);
encode_enter_regs([{freg,R}|Regs]) -> 
    ?SLJIT_ENTER_FLOAT(R) bor encode_enter_regs(Regs);
encode_enter_regs([{vreg,R}|Regs]) ->
    ?SLJIT_ENTER_VECTOR(R) bor encode_enter_regs(Regs).

decode_enter_regs(Regs) ->
    R = Regs band 16#ff,
    F = (Regs bsr 8) band 16#ff,
    V = (Regs bsr 16) band 16#ff,
    if R > 0 -> [{reg,R}]; true -> [] end ++
    if F > 0 -> [{freg,F}]; true -> [] end ++
    if V > 0 -> [{vreg,V}]; true -> [] end.

encode_ret(Ret) ->
    R = case Ret of
	    void   -> ?SLJIT_ARG_TYPE_RET_VOID;
	    word   -> ?SLJIT_ARG_TYPE_W;
	    word32 -> ?SLJIT_ARG_TYPE_32;
	    ptr    -> ?SLJIT_ARG_TYPE_P;
	    f64    -> ?SLJIT_ARG_TYPE_F64;
	    f32    -> ?SLJIT_ARG_TYPE_F32;
	    term   -> ?SLJIT_ARG_TYPE_TERM; %% returned as word
	    _ when is_integer(Ret) -> Ret
	end,
    ?SLJIT_ARG_RETURN(R).

decode_ret(Type) ->
    case Type band 16#f of
	?SLJIT_ARG_TYPE_RET_VOID -> void;
	?SLJIT_ARG_TYPE_W -> word;
	?SLJIT_ARG_TYPE_32 -> word32;
	?SLJIT_ARG_TYPE_P  -> ptr;
	?SLJIT_ARG_TYPE_TERM -> term;
	?SLJIT_ARG_TYPE_W_R -> word;
	?SLJIT_ARG_TYPE_32_R -> word32;
	?SLJIT_ARG_TYPE_P_R  -> ptr;
	?SLJIT_ARG_TYPE_TERM_R -> term;
	?SLJIT_ARG_TYPE_F64 -> f64;
	?SLJIT_ARG_TYPE_F32 -> f32
    end.
    
%% enter argtypes
encode_args(As) ->
    encode_args_(1, As).

encode_args_(_I, []) -> 0;
encode_args_(I, [Arg|As]) ->
    A = encode_arg(Arg),
    ?SLJIT_ARG_VALUE(A,I) bor encode_args_(I+1, As).

encode_arg(Arg) ->
    case Arg of
	word     -> ?SLJIT_ARG_TYPE_W;
	word32   -> ?SLJIT_ARG_TYPE_32;
	ptr      -> ?SLJIT_ARG_TYPE_P;
	term     -> ?SLJIT_ARG_TYPE_TERM;
	word_r   -> ?SLJIT_ARG_TYPE_W_R;
	word32_r -> ?SLJIT_ARG_TYPE_32_R;
	ptr_r    -> ?SLJIT_ARG_TYPE_P_R;
	term_r   -> ?SLJIT_ARG_TYPE_TERM_R;
	f64      -> ?SLJIT_ARG_TYPE_F64;
	f32      -> ?SLJIT_ARG_TYPE_F32;
	_ when is_integer(Arg) -> Arg
    end.

decode_args(Type) ->
    decode_args(Type bsr 4, []).

decode_args(0, As) -> 
    lists:reverse(As);
decode_args(Type, As) ->
    A = case Type band 16#f of
	    ?SLJIT_ARG_TYPE_W -> word;
	    ?SLJIT_ARG_TYPE_32 -> word32;
	    ?SLJIT_ARG_TYPE_TERM -> term;
	    ?SLJIT_ARG_TYPE_P -> ptr_r;
	    ?SLJIT_ARG_TYPE_W_R -> word_r;
	    ?SLJIT_ARG_TYPE_32_R -> word32_t;
	    ?SLJIT_ARG_TYPE_TERM_R -> term_r;
	    ?SLJIT_ARG_TYPE_P_R -> ptr_r;
	    ?SLJIT_ARG_TYPE_F64 -> f64;
	    ?SLJIT_ARG_TYPE_F32 -> f32
	end,
    decode_args(Type bsr 4, [A|As]).

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

decode_icall_type(Type) ->
    case Type band 16#ff of
	?SLJIT_CALL -> [call|decode_icall_flags(Type)];
	?SLJIT_CALL_REG_ARG -> [call_reg_arg|decode_icall_flags(Type)];
	_ -> decode_icall_flags(Type)
    end.

decode_icall_flags(Type) when Type band ?SLJIT_CALL_RETURN =:= 
			      ?SLJIT_CALL_RETURN ->
    [call_return];
decode_icall_flags(_) ->
    [].

encode_call_type([call|Ts]) ->
    ?SLJIT_CALL bor encode_call_type_flags(Ts);
encode_call_type([call_reg_arg|Ts]) -> 
    ?SLJIT_CALL_REG_ARG bor encode_call_type_flags(Ts);
encode_call_type(Type) when is_integer(Type) ->
    Type.

encode_call_type_flags([rewriteable_jump|Ts]) ->
    ?SLJIT_REWRITABLE_JUMP bor encode_call_type_flags(Ts);
encode_call_type_flags([call_return|Ts]) ->
    ?SLJIT_CALL_RETURN bor encode_call_type_flags(Ts);
encode_call_type_flags([]) ->
    0.

decode_call_type(Type) ->
    case Type band 16#ff of
	?SLJIT_CALL -> [call|decode_call_type_flags(Type)];
	?SLJIT_CALL_REG_ARG -> [call_reg_arg|decode_call_type_flags(Type)];
	_ -> decode_call_type_flags(Type)
    end.

decode_call_type_flags(Type) ->
    if Type band ?SLJIT_REWRITABLE_JUMP =:= ?SLJIT_REWRITABLE_JUMP ->
	    [rewriteable_jump];
       true ->
	    []
    end ++
    if Type band ?SLJIT_CALL_RETURN =:= ?SLJIT_CALL_RETURN ->
	    [call_return];
       true ->
	    []
    end.

%% translate short form registers to {r|s,I}
%% except sp since we do not know that number here or 
%% in a object file
-spec reg(R::reg()) -> reg().
reg(r0) -> {r,0};
reg(r1) -> {r,1};
reg(r2) -> {r,2};
reg(r3) -> {r,3};
reg(r4) -> {r,4};
reg(r5) -> {r,5};
reg(r6) -> {r,6};
reg(r7) -> {r,7};
reg(r8) -> {r,8};
reg(r9) -> {r,9};
reg(r10) -> {r,10};
reg(r11) -> {r,11};
reg(r12) -> {r,2};
reg(r13) -> {r,13};
reg(r14) -> {r,14};
reg(r15) -> {r,15};
reg({r,I}) when I >= 0, I =< 127 -> {r,I};
reg(s0) -> {s,0};
reg(s1) -> {s,1};
reg(s2) -> {s,2};
reg(s3) -> {s,3};
reg(s4) -> {s,4};
reg(s5) -> {s,5};
reg(s6) -> {s,6};
reg(s7) -> {s,7};
reg(s8) -> {s,8};
reg(s9) -> {s,9};
reg(s10) -> {s,10};
reg(s11) -> {s,11};
reg(s12) -> {s,12};
reg(s13) -> {s,13};
reg(s14) -> {s,14};
reg(s15) -> {s,15};
reg({s,I}) when I >= 0, I =< 127 -> {s,I};
reg(sp)    -> sp.

-spec freg(R::freg()) -> freg().
freg(fr0) -> {fr,0};
freg(fr1) -> {fr,1};
freg(fr2) -> {fr,2};
freg(fr3) -> {fr,3};
freg(fr4) -> {fr,4};
freg(fr5) -> {fr,5};
freg(fr6) -> {fr,6};
freg(fr7) -> {fr,7};
freg(fr8) -> {fr,8};
freg(fr9) -> {fr,9};
freg(fr10) -> {fr,10};
freg(fr11) -> {fr,11};
freg(fr12) -> {fr,12};
freg(fr13) -> {fr,13};
freg(fr14) -> {fr,14};
freg(fr15) -> {fr,15};
freg({fr,I}) when I >= 0, I =< 127 -> {fr,I};
freg(fs0) -> {fs,0};
freg(fs1) -> {fs,1};
freg(fs2) -> {fs,2};
freg(fs3) -> {fs,3};
freg(fs4) -> {fs,4};
freg(fs5) -> {fs,5};
freg(fs6) -> {fs,6};
freg(fs7) -> {fs,7};
freg(fs8) -> {fs,8};
freg(fs9) -> {fs,9};
freg(fs10) -> {fs,10};
freg(fs11) -> {fs,11};
freg(fs12) -> {fs,12};
freg(fs13) -> {fs,13};
freg(fs14) -> {fs,14};
freg(fs15) -> {fs,15};
freg({fs,I}) when I >= 0, I =< 127 -> {fs,I}.

-spec vreg(R::vreg()) -> vreg().
vreg(vr0) -> {vr,0};
vreg(vr1) -> {vr,1};
vreg(vr2) -> {vr,2};
vreg(vr3) -> {vr,3};
vreg(vr4) -> {vr,4};
vreg(vr5) -> {vr,5};
vreg(vr6) -> {vr,6};
vreg(vr7) -> {vr,7};
vreg(vr8) -> {vr,8};
vreg(vr9) -> {vr,9};
vreg(vr10) -> {vr,10};
vreg(vr11) -> {vr,11};
vreg(vr12) -> {vr,12};
vreg(vr13) -> {vr,13};
vreg(vr14) -> {vr,14};
vreg(vr15) -> {vr,15};
vreg({vr,I}) when I >= 0, I =< 127 -> {vr,I};
vreg(vs0) -> {vs,0};
vreg(vs1) -> {vs,1};
vreg(vs2) -> {vs,2};
vreg(vs3) -> {vs,3};
vreg(vs4) -> {vs,4};
vreg(vs5) -> {vs,5};
vreg(vs6) -> {vs,6};
vreg(vs7) -> {vs,7};
vreg(vs8) -> {vs,8};
vreg(vs9) -> {vs,9};
vreg(vs10) -> {vs,10};
vreg(vs11) -> {vs,11};
vreg(vs12) -> {vs,12};
vreg(vs13) -> {vs,13};
vreg(vs14) -> {vs,14};
vreg(vs15) -> {vs,15};
vreg({vs,I}) when I >= 0, I =< 127 -> {vs,I}.

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

