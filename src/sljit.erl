%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%% SLJIT - Simple Light JIT
%%% @end
%%% Created : 18 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit).

-on_load(init/0).
-export([get_platform_name/0]).
-export([create_compiler/0]).
-export([generate_code/1]).
-export([register_code/3]).
-export([unregister_code/2]).
-export([get_code/1]).
-export([disasm/1]).
-export([code_info/1, code_info/2]).
-export([call_code/1, call_code/2, call_code/3, call_code/4, call_code/5]).
-export([emit_op0/2]).
-export([emit_op1/6]).
-export([emit_op2/8]).
-export([emit_op2u/6]).
-export([emit_op2r/7]).
-export([emit_shift_into/7]).
-export([emit_op_src/4]).
-export([emit_op_dst/4]).
-export([emit_fop1/6]).
-export([emit_fop2/8]).
-export([emit_fop2r/7]).
-export([emit_fset32/3]).
-export([emit_fset64/3]).
-export([emit_fcopy/4]).
-export([label_name/2]).
-export([module/2]).
-export([function/2]).
-export([emit_label/1]).
-export([emit_jump/2]).
-export([emit_call/3]).
-export([emit_cmp/6]).
-export([emit_fcmp/6]).
-export([set_label/2]).
-export([set_target/2]).
-export([emit_ijump/4]).
-export([emit_icall/5]).
-export([emit_mcall/5]).
-export([emit_mjump/4]).
-export([emit_enter/6]).
-export([set_context/6]).
-export([emit_return/4]).
-export([emit_return_void/1]).
-export([emit_return_to/3]).
-export([emit_simd_op2/6]).

%% emit_op_flags(struct sljit_compiler *compiler, sljit_s32 op,	sljit_s32 dst, sljit_sw dstw, sljit_s32 type);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_select(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 dst_reg, sljit_s32 src1, sljit_sw src1w, sljit_s32 src2_reg);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_fselect(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 dst_freg, sljit_s32 src1, sljit_sw src1w, sljit_s32 src2_freg);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_mem(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 reg, sljit_s32 mem, sljit_sw memw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_mem_update(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 reg, sljit_s32 mem, sljit_sw memw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_fmem(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 freg, sljit_s32 mem, sljit_sw memw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_fmem_update(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 freg, sljit_s32 mem, sljit_sw memw);

%% SIMD Operations
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_mov(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 srcdst, sljit_sw srcdstw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_replicate(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 src, sljit_sw srcw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_lane_mov(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 lane_index, sljit_s32 srcdst, sljit_sw srcdstw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_lane_replicate(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 src, sljit_s32 src_lane_index);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_extend(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 src, sljit_sw srcw);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_simd_sign(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 vreg, sljit_s32 dst, sljit_sw dstw);


%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_atomic_load(struct sljit_compiler *compiler, sljit_s32 op, sljit_s32 dst_reg, sljit_s32 mem_reg);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_atomic_store(struct sljit_compiler *compiler, sljit_s32 op, sljit_s32 src_reg, sljit_s32 mem_reg, sljit_s32 temp_reg);
%% SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_get_local_base(struct sljit_compiler *compiler, sljit_s32 dst, sljit_sw dstw, sljit_sw offset);
%% SLJIT_API_FUNC_ATTRIBUTE struct sljit_const* sljit_emit_const(struct sljit_compiler *compiler, sljit_s32 dst, sljit_sw dstw, sljit_sw init_value);
%% SLJIT_API_FUNC_ATTRIBUTE struct sljit_jump* sljit_emit_mov_addr(struct sljit_compiler *compiler, sljit_s32 dst, sljit_sw dstw);

-type compiler() :: reference().
-type code() :: reference() | {Mod::atom(), Func::atom()}.
-type label() :: reference().
-type jump() :: reference().
-type unsigned() :: non_neg_integer().

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(sljit), "sljit_nif"),
    erlang:load_nif(Nif, 0).

-spec create_compiler() -> compiler().
create_compiler() ->
    ?nif_stub().

-spec get_platform_name() -> string().
get_platform_name() ->
    ?nif_stub().

-spec generate_code(compiler()) -> code().
generate_code(_Compiler) ->
    ?nif_stub().


-spec register_code(code(), Mod::atom(), Func::atom()) ->
	  ok | {error, ealready}.

register_code(_Code, _Mod, _Func) ->
    ?nif_stub().

-spec unregister_code(Mod::atom(), Func::atom()) ->
	  ok | {error, ealready}.

unregister_code(_Mod, _Func) ->
    ?nif_stub().


-type code_info_key() :: code_size | exec_offset | 
			 argc | return_type | {arg_type,1..4}.

-spec code_info(code(), Key::code_info_key()) -> term().
code_info(_Code, _Info) ->
    ?nif_stub().

-spec code_info(code()) -> [{Key::code_info_key(),Value::term()}].
code_info(Code) ->
    [{Key, code_info(Code, Key)} || 
	Key <- [code_size, exec_offset, argc, return_type, 
		{arg_type,1},{arg_type,2},{arg_type,3},{arg_type,4}]].

-spec get_code(code()) -> binary().
get_code(Code) ->
    code_info(Code, code).

-spec disasm(code()) -> term().
disasm(Code) ->
    Bin = get_code(Code),
    disasm:bin(x86_64, [{mode,64}], Bin).

-type arg_type() :: integer() | float().
-type ret_type() :: ok | integer() | float().

-spec call_code(Code::code()) -> ret_type().
call_code(_Code) ->
    ?nif_stub().
-spec call_code(Code::code(),A1::arg_type()) -> ret_type().
call_code(_Code,_A1) ->
    ?nif_stub().
-spec call_code(Code::code(),A1::arg_type(),A2::arg_type()) -> ret_type().
call_code(_Code,_A1,_A2) ->
    ?nif_stub().
-spec call_code(Code::code(),A1::arg_type(),A2::arg_type(),A3::arg_type()) -> 
	  ret_type().
call_code(_Code,_A1,_A2,_A3) ->
    ?nif_stub().
-spec call_code(Code::code(),A1::arg_type(),A2::arg_type(),
		A3::arg_type(),A4::arg_type()) -> ret_type().
call_code(_Code,_A1,_A2,_A3,_A4) ->
    ?nif_stub().


-spec emit_op0(compiler(), Op::integer()) -> ok.
emit_op0(_Compiler, _Op) ->
    ?nif_stub().

-spec emit_op1(compiler(), Op::integer(),
	       Dst::integer(), Dstw::integer(),
	       Src::integer(), Srcw::integer()) -> ok.
emit_op1(_Compiler, _Op, _Dst, _Dstw, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_op2(compiler(), Op::integer(),
	       Dst::integer(), Dstw::integer(),
	       Src1::integer(), Src1w::integer(),
	       Src2::integer(), Src2w::integer()) -> ok.
emit_op2(_Compiler, _Op, _Dst, _Dstw, _Src1, _Src1w, _Src2, _Src2w) ->
    ?nif_stub().

-spec emit_op2u(compiler(), Op::integer(),
		Src1::integer(), Src1w::integer(),
		Src2::integer(), Src2w::integer()) -> ok.
emit_op2u(_Compiler, _Op, _Src1, _Src1w, _Src2, _Src2w) ->
    ?nif_stub().

-spec emit_op2r(compiler(), Op::integer(),
		DstReg::integer(),
		Src1::integer(), Src1w::integer(),
		Src2::integer(), Src2w::integer()) -> ok.

emit_op2r(_Compiler, _Op, _DstReg, _Src1, _Src1w, _Src2, _Src2w) ->
    ?nif_stub().

-spec emit_shift_into(compiler(), Op::integer(),
		      DstReg::integer(),
		      Src1Reg::integer(), Src2Reg::integer(),
		      Src3::integer(), Src3w::integer()) -> ok.

emit_shift_into(_Compiler, _Op, _DstReg, _Src1Reg, _Src2Reg, _Src3, _Src3w) ->
    ?nif_stub().

-spec emit_op_src(compiler(), Op::integer(), Src::integer(), Srcw::integer()) ->
	  ok.
emit_op_src(_Compiler, _Op, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_op_dst(compiler(), Op::integer(), Dst::integer(), Dstw::integer()) ->
	  ok.
emit_op_dst(_Compiler, _Op, _Dst, _Dstw) ->
    ?nif_stub().

-spec emit_fop1(compiler(), Op::integer(), Dst::integer(), Dstw::integer(),
		Src::integer(), Srcw::integer()) -> ok.
emit_fop1(_Compiler, _Op, _Dst, _Dstw, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_fop2(compiler(), Op::integer(), Dst::integer(), Dstw::integer(),
		Src1::integer(), Src1w::integer(),
		Src2::integer(), Src2w::integer()) ->
	   ok.
		
emit_fop2(_Compiler, _Op, _Dst, _Dstw, _Src1, _Src1w, _Src2, _Src2w) ->
    ?nif_stub().

-spec emit_fop2r(compiler(), Op::integer(), DstReg::integer(),
		 Src1::integer(), Src1w::integer(),
		 Src2::integer(), Src2w::integer()) ->
	  ok.
emit_fop2r(_Compiler, _Op, _DstReg, _Src1, _Src1w, _Src2, _Src2w) ->
	?nif_stub().

-spec emit_fset32(compiler(), FReg::integer(), Value::float()) -> ok.
emit_fset32(_Compiler, _FReg, _Value) ->
    ?nif_stub().

-spec emit_fset64(compiler(), FReg::integer(), Value::float()) -> ok.
emit_fset64(_Compiler, _FReg, _Value) ->
    ?nif_stub().

-spec emit_fcopy(compiler(), Op::integer(), FReg::integer(), Reg::integer()) ->
	  ok.
emit_fcopy(_Compiler, _OP, _FReg, _Reg) ->
    ?nif_stub().

%% Special synthic instructions only for object generation
%% FIXME: Label must be integer right now but should be term!
%% possibly prefix label with label type?
-spec label_name(compiler(), Label::term()) -> ok.
label_name(_Compiler, _Label) ->
    ok.

-spec module(compiler(), Name::atom()) -> ok.
module(_Compiler, _Name) ->
    ok.

-spec function(compiler(), Name::atom()) -> ok.
function(_Compiler, _Name) ->
    ok.

-spec emit_label(compiler()) -> label().
emit_label(_Compiler) ->
    ?nif_stub().

-spec emit_jump(compiler(), _Type::integer()) -> jump().
emit_jump(_Compiler, _Type) ->
    ?nif_stub().

-spec emit_call(compiler(), _Type::integer(), _ArgTypes::integer()) -> jump().
emit_call(_Compiler, _Type, _ArgTypes) ->
    ?nif_stub().

-spec emit_mcall(compiler(), _Type::integer(), _ArgTypes::integer(), 
		 Mod::atom(), Fun::atom()) -> jump().
emit_mcall(_Compiler, _Type, _ArgTypes, _Mod, _Fun) ->
    ?nif_stub().

-spec emit_cmp(compiler(), _Type::integer(), _Src1::integer(), _Src1w::integer(), _Src2::integer(), _Src2w::integer()) -> jump().
emit_cmp(_Compiler, _Type, _Src1, _Src1w, _Src2, _Src2w) ->
    ?nif_stub().

-spec emit_fcmp(compiler(), Type::integer(), Src1::integer(), Src1w::integer(),
		Src2::integer(), Src2w::integer()) -> jump().
emit_fcmp(_Compiler, _Type, _Src1, _Src1w, _Src2, _Src2w) ->
	?nif_stub().

-spec set_label(jump(), label()) -> ok.
set_label(_Jump, _Label) ->
	?nif_stub().

-spec set_target(jump(), Target::unsigned()) -> ok.
set_target(_Jump, _Target) ->
    ?nif_stub().

-spec emit_ijump(compiler(), Type::integer(), 
		 Src::integer(), Srcw::integer()) -> jump().
emit_ijump(_Compiler, _Type, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_mjump(compiler(), Type::integer(),
		 Mod::atom(), Fun::atom()) -> ok | {error,term()}.
emit_mjump(_Compiler, _Type, _Mod, _Fun) ->
    ?nif_stub().

-spec emit_icall(compiler(), Type::integer(), ArgTypes::integer(),
		 Src::integer(), Srcw::integer()) -> ok | {error, term()}.
emit_icall(_Compiler, _Type, _ArgTypes, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_enter(compiler(), Options::integer(), 
		 ArgTypes::integer(), Scratches::integer(), 
		 Saveds::integer(), LocalSize::integer()) -> ok.
emit_enter(_Compiler, _Options, _ArgTypes, _Scratches, _Saveds, _LocalSize) ->
    ?nif_stub().

-spec set_context(compiler(), Options::integer(), 
		  ArgTypes::integer(), Scratches::integer(), 
		  Saveds::integer(), LocalSize::integer()) -> ok.
set_context(_Compiler, _Options, _ArgTypes, _Scratches, _Saveds, _LocalSize) ->
    ?nif_stub().

-spec emit_return(compiler(), Op::integer(),
		  Src::integer(), Srcw::integer()) -> ok.
emit_return(_Compiler, _Op, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_return_void(compiler()) -> ok.
emit_return_void(_Compiler) ->
    ?nif_stub().

-spec emit_return_to(compiler(), Src::integer(), Srcw::integer()) -> ok.
emit_return_to(_Compiler, _Src, _Srcw) ->
    ?nif_stub().

-spec emit_simd_op2(compiler(), Type::integer(), DstVReg::integer(), Src1VReg::integer(), Src2::integer(), Src2w::integer()) -> ok.
emit_simd_op2(_Compiler, _Type, _DstVreg, _Src1VReg, _Src2, _Src2w) -> 
    ?nif_stub().
