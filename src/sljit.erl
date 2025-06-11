%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%% SLJIT - Simple Light JIT
%%% @end
%%% Created : 18 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit).

-on_load(init/0).
-export([get_platform_name/0]).
-export([get_platform_name/1]).
-export([get_platform_info/0]).
-export([cpu_features/0, cpu_features/1]).
-export([cpu_feature/1, cpu_feature/2]).
-export([has_cpu_feature/1, has_cpu_feature/2]).
-export([create_compiler/0]).
-export([create_compiler/1]).
-export([generate_code/1]).
-export([unregister_code/1]).
-export([get_code/1]).
-export([disasm/1]).
-export([code_info/1, code_info/2]).
-export([call/1, call/2, call/3, call/4, call/5]).
-export([emit_op_src/3]).
-export([emit_op_dst/3]).
-export([emit_op0/2]).
-export([emit_op1/4]).
-export([emit_op2u/4]).
-export([emit_op2/5]).
-export([emit_op2r/5]).
-export([emit_shift_into/6]).

-export([emit_fop1/4]).
-export([emit_fop2/5]).
-export([emit_fop2r/5]).
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
-export([emit_cmp/4]).
-export([emit_fcmp/4]).
-export([set_label/2]).
-export([set_target/2]).
-export([emit_ijump/3]).
-export([emit_icall/4]).
-export([emit_mcall/5]).
-export([emit_mjump/4]).
-export([emit_enter/6]).
-export([set_context/6]).
-export([emit_return/3]).
-export([emit_return_void/1]).
-export([emit_return_to/2]).
-export([emit_simd_op2/5]).
-export([emit_simd_mov/4]).
-export([get_label_addr/1]).
-export([emit_const/4]).
-export([set_constant/2, set_constant/3]).
-export([emit_op_addr/3]).
-export([set_jump/2, set_jump/3]).
%% new operations
-export([emit_simd_arith_op2/5]).

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

-export([create_memory/1]).
-export([read_memory/1, read_memory/2, read_memory/3]).
-export([write_memory/2, write_memory/3, write_memory/4]).

-type compiler() :: reference().
-type code() :: reference() | Mod::atom() | {Mod::atom(), Func::atom()} | 
		{reference(), Func::atom()}.
-type label() :: reference().
-type jump() :: reference().
-type const() :: reference().
-type unsigned() :: non_neg_integer().
-type memory() :: reference().

-type arch() :: x86_32 | x86_64 | arm_v6 | arm_v7 | arm_thumb2 | arm_64 |
		ppc_32 | ppc_64 | mips_32 | mips_64 | riscv_32 | riscv_64 |
		s390x | loongarch_64 | emulator.

-type gp_reg() :: r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7 | r8 | r9 |
		  r10 | r11 | r12 | r13 | r14 | r15 | {r, 0..255}.
-type saved_reg() :: s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | s8 | s9 |
		     s10 | s11 | s12 | s13 | s14 | s15 | {s, 0..255}.
-type reg() :: gp_reg() | saved_reg() | sp.

-type float_reg() :: 
	fr0 | fr1 | fr2 | fr3 | fr4 | fr5 | fr6 | fr7 | fr8 | fr9 |
	fr10 | fr11 | fr12 | fr13 | fr14 | fr15 | {fr, 0..255}.
-type float_saved_reg() ::
	fs0 | fs1 | fs2 | fs3 | fs4 | fs5 | fs6 | fs7 | fs8 | fs9 |
	fs10 | fs11 | fs12 | fs13 | fs14 | fs15 | {fs, 0..255}.
-type freg() :: float_reg() | float_saved_reg().

-type vec_reg() :: vr0 | vr1 | vr2 | vr3 | vr4 | vr5 | vr6 | vr7 | vr8 | vr9 |
		   vr10 | vr11 | vr12 | vr13 | vr14 | vr15 | {vr, 0..255}.
-type vec_saved_reg() ::
	vs0 | vs1 | vs2 | vs3 | vs4 | vs5 | vs6 | vs7 | vs8 | vs9 | 
	vs10 | vs11 | vs12 | vs13 | vs14 | vs15 | {vs, 0..255}.
-type vreg() :: vec_reg() | vec_saved_reg().

-type named_constant() :: vsize.
 
-type value() :: integer() | named_constant().
-type scale() :: 0|1|2|3.  %% = 1,2,4,8

-type mem() :: {mem, value()} |                %% [imm]
	       {mem, reg()} |                  %% [reg]
	       {mem, reg(), value()} |         %% [reg + imm]
	       {mem, reg(), reg()} |           %% [reg + reg]
	       {mem, reg(), reg(), scale()}.   %% [reg + (reg<<imm)]

-type imm() :: {imm, value()}.

-type dst() :: mem() | reg().
-type src() :: mem() | reg() | imm().

-type fdst() :: mem() | freg().
-type fsrc() :: mem() | freg().

%%-type vdst() :: mem() | vreg().
-type vsrc() :: mem() | vreg().
-type vsrcdst() :: mem() | reg().  %% memory location or pointer

-export_type([gp_reg/0, saved_reg/0, reg/0]).
-export_type([float_reg/0, float_saved_reg/0, freg/0]).
-export_type([vec_reg/0, vec_saved_reg/0, vreg/0]).
-export_type([imm/0, mem/0, dst/0, src/0]).

-include("../include/sljit.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(sljitter), "sljit_nif"),
    erlang:load_nif(Nif, 0).

-spec create_compiler() -> compiler().
create_compiler() ->
    ?nif_stub().

-spec create_compiler(Arch::arch()) -> compiler().
create_compiler(_Arch) ->
    ?nif_stub().

-spec get_platform_name() -> [{Arch::arch(), Info::string()}].
get_platform_name() ->
    ?nif_stub().

-spec get_platform_name(Arch::arch()) ->
	  {Arch::arch(), Info::string()} | [{Arch::arch(), Info::string()}].
get_platform_name(_Arch) ->
    ?nif_stub().

-type platform_info() :: #{ number_of_registers => integer(),
			    number_of_scratch_registers  => integer(),
			    number_of_saved_registers  => integer(),
			    number_of_float_registers  => integer(),
			    number_of_saved_float_registers  => integer(),
			    number_of_vector_registers  => integer(),
			    number_of_saved_vector_registers  => integer(),
			    return_reg  => integer(),
			    sp  => integer(),
			    vsize  => integer(),
			    name => string() }.
-spec get_platform_info() -> 
	  #{ atom() => platform_info() }.
get_platform_info() ->
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
has_cpu_feature(Feature) ->
    has_cpu_feature(native, Feature).

-spec has_cpu_feature(Arch::arch(), Feature::integer()) ->
	  boolean().
has_cpu_feature(_Arch, _Feature) ->
    ?nif_stub().

cpu_features() ->
    cpu_features(native).

cpu_features(Arch) ->
    [ {Name, has_cpu_feature(Arch, Flag)} ||
	{Name,Flag} <- maps:to_list(feature_map()) ].


cpu_feature(Name) when is_atom(Name) ->
    cpu_feature(native, Name).
cpu_feature(Arch,Name) when is_atom(Arch), is_atom(Name) ->
    case maps:find(Name, feature_map()) of
	{ok, F} -> has_cpu_feature(Arch, F);
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

%% call memory argument handle
-spec create_memory(Size::integer()) -> memory().
create_memory(_Size) ->
    ?nif_stub().    

-spec read_memory(Mem::memory()) -> binary().
read_memory(_Mem) ->
    ?nif_stub().

-spec read_memory(Mem::memory(), Pos::integer()) -> binary().
read_memory(_Mem, _Pos) ->
    ?nif_stub().

-spec read_memory(Mem::memory(), Pos::integer(), Size::integer()) -> binary().
read_memory(_Mem, _Pos, _Size) ->
    ?nif_stub().

-spec write_memory(Mem::memory(), Data::binary()) -> 
	  Written::integer().
write_memory(_Mem, _Data) ->
    ?nif_stub().

-spec write_memory(Mem::memory(),Pos::integer(), 
		   Data::binary()) ->
	  Written::integer().
write_memory(_Mem, _Pos, _Data) ->
    ?nif_stub().

-spec write_memory(Mem::memory(), Pos::integer(), Size::integer(),
		   Data::binary()) ->
	  Written::integer().
write_memory(_Mem, _Pos, _Size, _Data) ->
    ?nif_stub().

-type arg_type() :: integer() | float() | binary() | memory().
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
	       Dst::dst(), Src::src()) -> ok.
emit_op1(_Compiler, _Op, _Dst, _Src) ->
    ?nif_stub().

-spec emit_op2(compiler(), Op::integer(),
	       Dst::dst(), Src1::src(), Src2::src()) -> ok.
emit_op2(_Compiler, _Op, _Dst, _Src1, _Src2) ->
    ?nif_stub().

-spec emit_op2u(compiler(), Op::integer(),
		Src1::src(), Src2::src()) -> ok.
emit_op2u(_Compiler, _Op, _Src1, _Src2) ->
    ?nif_stub().

-spec emit_op2r(compiler(), Op::integer(),
		Dst::reg(), Src1::src(), Src2::src()) -> ok.

emit_op2r(_Compiler, _Op, _Dst, _Src1, _Src2) ->
    ?nif_stub().

-spec emit_shift_into(compiler(), Op::integer(),
		      Dst::reg(), Src1::reg(), Src2::reg(), Src3::src()) -> ok.

emit_shift_into(_Compiler, _Op, _Dst, _Src1, _Src2, _Src3) ->
    ?nif_stub().

-spec emit_op_src(compiler(), Op::integer(), Src::src()) ->
	  ok.
emit_op_src(_Compiler, _Op, _Src) ->
    ?nif_stub().

-spec emit_op_dst(compiler(), Op::integer(), Dst::dst()) ->
	  ok.
emit_op_dst(_Compiler, _Op, _Dst) ->
    ?nif_stub().

-spec emit_fop1(compiler(), Op::integer(),Dst::fdst(),Src::fsrc()) -> ok.
emit_fop1(_Compiler, _Op, _Dst, _Src) ->
    ?nif_stub().

-spec emit_fop2(compiler(), Op::integer(), 
		Dst::fdst(),Src1::fsrc(),Src2::fsrc()) ->
	   ok.
		
emit_fop2(_Compiler, _Op, _Dst, _Src1, _Src2) ->
    ?nif_stub().

-spec emit_fop2r(compiler(), Op::integer(), 
		 Dst::freg(),Src1::fsrc(), Src2::fsrc()) ->
	  ok.
emit_fop2r(_Compiler, _Op, _Dst, _Src1, _Src2) ->
	?nif_stub().

-spec emit_fset32(compiler(), FReg::freg(), Value::float()) -> ok.
emit_fset32(_Compiler, _FReg, _Value) ->
    ?nif_stub().

-spec emit_fset64(compiler(), FReg::freg(), Value::float()) -> ok.
emit_fset64(_Compiler, _FReg, _Value) ->
    ?nif_stub().

-spec emit_fcopy(compiler(), Op::integer(), FReg::freg(), Reg::reg()) ->
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
-spec function(compiler(), Name::atom()) -> label().
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

-spec emit_cmp(compiler(), _Type::integer(), 
	       _Src1::src(), _Src2::src()) -> jump().
emit_cmp(_Compiler, _Type, _Src1, _Src2) ->
    ?nif_stub().

-spec emit_fcmp(compiler(), Type::integer(), 
		Src1::fsrc(), Src2::fsrc()) -> jump().
emit_fcmp(_Compiler, _Type, _Src1, _Src2) ->
	?nif_stub().

-spec set_label(jump(), label()) -> ok.
set_label(_Jump, _Label) ->
    ?nif_stub().

-spec set_target(jump(), Target::unsigned()) -> ok.
set_target(_Jump, _Target) ->
    ?nif_stub().

-spec emit_ijump(compiler(), Type::integer(), 
		 Src::src()) -> jump().
emit_ijump(_Compiler, _Type, _Src) ->
    ?nif_stub().

-spec emit_mjump(compiler(), Type::integer(),
		 Mod::atom(), Fun::atom()) -> ok | {error,term()}.
emit_mjump(_Compiler, _Type, _Mod, _Fun) ->
    ?nif_stub().

-spec emit_icall(compiler(), Type::integer(), ArgTypes::integer(),
		 Src::src()) -> ok | {error, term()}.
emit_icall(_Compiler, _Type, _ArgTypes, _Src) ->
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

-spec emit_return(compiler(), Op::integer(), Src::integer()) -> ok.
emit_return(_Compiler, _Op, _Src) ->
    ?nif_stub().

-spec emit_return_void(compiler()) -> ok.
emit_return_void(_Compiler) ->
    ?nif_stub().

-spec emit_return_to(compiler(), Src::integer()) -> ok.
emit_return_to(_Compiler, _Src) ->
    ?nif_stub().

-spec emit_simd_op2(compiler(), Type::integer(), 
		    DstVReg::vreg(), Src1::vreg(), Src2::vsrc()) -> ok.
emit_simd_op2(_Compiler, _Type, _Dst, _Src1, _Src2) -> 
    ?nif_stub().

-spec emit_simd_arith_op2(compiler(), Type::integer(), 
			  DstVReg::vreg(), Src1::vreg(), Src2::vsrc()) -> ok.
emit_simd_arith_op2(_Compiler, _Type, _Dst, _Src1, _Src2) -> 
    ?nif_stub().

-spec emit_simd_mov(compiler(), Type::integer(), 
		    VReg::vreg(), SrcDst::vsrcdst()) -> ok.
emit_simd_mov(_Compiler, _Type, _VReg, _SrcDst) -> 
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
-spec emit_const(compiler(), Op::integer(),Dst::reg(),
		 InitValue::integer()|atom()) -> const().
emit_const(_Compiler, _Op, _Dst, _InitValue) -> 
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
-spec emit_op_addr(Compiler::compiler(),  Op::integer(), Dst::integer()) ->
	  jump().

emit_op_addr(_Compiler, _Op, _Dst) ->
    ?nif_stub().

-spec set_jump({CodeOrMod::code()|atom(), Name::atom()}, NewTarget::atom()) ->
	  ok.
set_jump({CodeOrMod,JumpName}, NewTarget) ->
    set_jump(CodeOrMod, JumpName, NewTarget).

-spec set_jump(CodeOrMod::code()|atom(), JumpName::atom(), NewTarget::atom()) ->
	  ok.

set_jump(_CodeOrMod,_JumpName,_NewTarget) ->
    ?nif_stub().
