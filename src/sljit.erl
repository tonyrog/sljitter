%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%% SLJIT - Simple Light JIT
%%% @end
%%% Created : 18 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit).

-on_load(init/0).
-export([get_platform_name/0]).
-export([cpu_features/0]).
-export([cpu_feature/1]).
-export([has_cpu_feature/1]).
-export([create_compiler/0]).
-export([generate_code/1]).
-export([unregister_code/1]).
-export([get_code/1]).
-export([disasm/1]).
-export([code_info/1, code_info/2]).
-export([call/1, call/2, call/3, call/4, call/5]).
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
-export([const_name/2]).
-export([jump_name/2]).
-export([module/2]).
-export([function/2]).
-export([constant/3]).
-export([label_addr/3]).
-export([jump_addr/3]).
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
-export([get_label_addr/1]).
-export([emit_const/5]).
-export([set_constant/2, set_constant/3]).
-export([emit_op_addr/4]).
-export([set_jump/2, set_jump/3]).

%% -export([emit_select/6]).
%% -export([emit_fselect/6]).
%% -export([emit_mem/5]).
%% -export([emit_mem_update/5]).
%% -export([emit_fmem/5]).
%% -export([emit_fmem_update/5]).
%% -export([emit_simd_move/4]).
%% -export([emit_simd_replicate/5]).
%% -export([emit_simd_lane_mov/6]).
%% -export([emit_simd_lane_replicate/5]).
%% -export([emit_simd_extend/5]).
%% -export([emit_simd_sign/4]).


-type compiler() :: reference().
-type code() :: reference() | Mod::atom() | {Mod::atom(), Func::atom()} | 
		{reference(), Func::atom()}.
-type label() :: reference().
-type jump() :: reference().
-type const() :: reference().
-type unsigned() :: non_neg_integer().

-include("../include/sljit.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(sljitter), "sljit_nif"),
    erlang:load_nif(Nif, 0).

-spec create_compiler() -> compiler().
create_compiler() ->
    ?nif_stub().

-spec get_platform_name() -> string().
get_platform_name() ->
    ?nif_stub().

feature_map() ->
    #{ has_fpu => ?SLJIT_HAS_FPU,
       has_virtual_registers => ?SLJIT_HAS_VIRTUAL_REGISTERS,
       has_zero_register => ?SLJIT_HAS_ZERO_REGISTER,
       has_clz => ?SLJIT_HAS_CLZ,
       has_ctz => ?SLJIT_HAS_CTZ,
       has_rev => ?SLJIT_HAS_REV,
       has_rot => ?SLJIT_HAS_ROT,
       has_cmov => ?SLJIT_HAS_CMOV,
       has_prefetch => ?SLJIT_HAS_PREFETCH,
       has_copy_f32 => ?SLJIT_HAS_COPY_F32,
       has_copy_f64 => ?SLJIT_HAS_COPY_F64,
       has_f64_as_f32_pair => ?SLJIT_HAS_F64_AS_F32_PAIR,
       has_simd => ?SLJIT_HAS_SIMD,
       simd_regs_are_pairs => ?SLJIT_SIMD_REGS_ARE_PAIRS,
       has_atomic => ?SLJIT_HAS_ATOMIC,
       has_memory_barrier => ?SLJIT_HAS_MEMORY_BARRIER,
       has_avx => ?SLJIT_HAS_AVX,
       has_avx2 => ?SLJIT_HAS_AVX2,
       has_lasx => ?SLJIT_HAS_LASX
     }.


-spec has_cpu_feature(Feature::integer()) ->
	  boolean().
has_cpu_feature(_Feature) ->
    ?nif_stub().

cpu_features() ->
    [ {Name, has_cpu_feature(Flag)} || {Name,Flag} <- maps:to_list(feature_map()) ].

cpu_feature(Name) when is_atom(Name) ->
    case maps:find(Name, feature_map()) of
	{ok, F} -> has_cpu_feature(F);
	error -> exit(no_such_feature)
    end.

-spec generate_code(compiler()) -> code().
generate_code(_Compiler) ->
    ?nif_stub().

-spec unregister_code(Mod::atom()) ->
	  ok | {error, ealready}.

unregister_code(_Mod) ->
    ?nif_stub().


-type code_info_key() :: code_size | exec_offset | 
			 argc | return_type | {arg_type,1..4} |
			 addr_list | label_list | jump_list | const_list.

-spec code_info(code(), Key::code_info_key()) -> term().
code_info(_Code, _Info) ->
    ?nif_stub().

-spec code_info(code()) -> [{Key::code_info_key(),Value::term()}].
code_info(Code) when is_atom(Code); is_reference(Code) ->
    [{Key, code_info(Code, Key)} || 
	Key <- [code_size, exec_offset, 
		label_list, jump_list, const_list, export_list
	       ]];
code_info(Code={Mod,Fun}) when is_atom(Mod), is_atom(Fun);
			       is_reference(Mod), is_atom(Fun) ->
    [{Key, code_info(Code, Key)} ||     
	Key <- [argc, return_type, 
		{arg_type,1},{arg_type,2},{arg_type,3},{arg_type,4}]].


-spec get_code(code()) -> binary().
get_code(Code) ->
    code_info(Code, code).

-spec disasm(code()) -> term().
disasm(Code) ->
    Bin = get_code(Code),
    disasm:bin(x86_64, [{mode,64}], Bin).

-type arg_type() :: integer() | float() | binary().
-type ret_type() :: ok | integer() | float().

-spec call(Code::code()) -> ret_type().
call(_Code) ->
    ?nif_stub().
-spec call(Code::code(),A1::arg_type()) -> ret_type().
call(_Code,_A1) ->
    ?nif_stub().
-spec call(Code::code(),A1::arg_type(),A2::arg_type()) -> ret_type().
call(_Code,_A1,_A2) ->
    ?nif_stub().
-spec call(Code::code(),A1::arg_type(),A2::arg_type(),A3::arg_type()) -> 
	  ret_type().
call(_Code,_A1,_A2,_A3) ->
    ?nif_stub().
-spec call(Code::code(),A1::arg_type(),A2::arg_type(),
	   A3::arg_type(),A4::arg_type()) -> ret_type().
call(_Code,_A1,_A2,_A3,_A4) ->
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

-spec const_name(compiler(), Name::term()) -> ok.
const_name(_Compiler, _Name) ->
    ok.

-spec jump_name(compiler(), Label::term()) -> ok.
jump_name(_Compiler, _Label) ->
    ok.

%% Set current module name
-spec module(compiler(), Name::atom()) -> ok.
module(_Compiler, _Name) ->
    ?nif_stub().

%% Set current function name
-spec function(compiler(), Name::atom()) -> ok.
function(_Compiler, _Name) ->
    ?nif_stub().

%% create runtime constant 
-spec constant(compiler(), Name::atom(), _Const::const()) -> ok.
constant(_Compiler, _Name, _Const) ->
    ?nif_stub().

-spec emit_label(compiler()) -> label().
emit_label(_Compiler) ->
    ?nif_stub().

%% create runtime label (jump target)
-spec label_addr(compiler(), Name::atom(), _Label::label()) -> ok.
label_addr(_Compiler, _Name, _Label) ->
    ?nif_stub().

%% create runtime jump (source)
-spec jump_addr(compiler(), Name::atom(), _Jump::jump()) -> ok.
jump_addr(_Compiler, _Name, _Jump) ->
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

-spec get_label_addr(Label::label()) -> integer().
get_label_addr(_Label) ->
    ?nif_stub().    

%% op is one of
%% SLJIT_MOV, 
%% SLJIT_MOV32,
%% SLJIT_MOV_S32,
%% SLJIT_MOV_U8,   (init value may be 9 signed bits????)
%% SLJIT_MOV32_U8
-spec emit_const(compiler(), Op::integer(),
		 Dst::integer(), Dstw::integer(), 
		 InitValue::integer()|atom()) -> const().
emit_const(_Compiler, _Op, _Dst, _DstW, _InitValue) -> 
    ?nif_stub().


set_constant({CodeorMod,Name}, NewConstant) ->
    set_constant(CodeorMod, Name, NewConstant).

-spec set_constant(CodeOrMod::code()|atom(), Name::atom(), 
		   NewConstant::integer()) -> ok.
set_constant(_CodeOrMod, _Name, _NewConstant) ->
    ?nif_stub().

%% op is one off
%% SLJIT_MOV_ADDR     -- The address is suitable for jump/call target.
%% SLJIT_MOV_ABS_ADDR -- The address is suitable for reading memory.
-spec emit_op_addr(Compiler::compiler(),  Op::integer(),
		   Dst::integer(), Dstw::integer()) ->
	  jump().

emit_op_addr(_Compiler, _Op, _Dst, _Dstw) ->
    ?nif_stub().

-spec set_jump({CodeOrMod::code()|atom(), Name::atom()}, NewTarget::atom()) ->
	  ok.
set_jump({CodeOrMod,JumpName}, NewTarget) ->
    set_jump(CodeOrMod, JumpName, NewTarget).

-spec set_jump(CodeOrMod::code()|atom(), JumpName::atom(), NewTarget::atom()) ->
	  ok.

set_jump(_CodeOrMod,_JumpName,_NewTarget) ->
    ?nif_stub().
