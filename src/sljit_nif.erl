%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    BEAM tag scheme
%%% @end
%%% Created :  4 Feb 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit_nif).

-export([test_small/3]).
-export([test_tuple/3]).
-export([test_float/3]).
-export([load_float/2]).
-export([get_float/4]).
-export([make_efloat/0, load_efloat/0]).

-include("../include/sljit_nif.hrl").

%%  return a jump r0 input, r0 is consumed
test_small(R0,R1,Fail) ->
    [{'and', R1, R0, ?TAG_IMMED1_MASK},
     {jump, {cmp, not_equal, R1, ?TAG_IMMED1_SMALL}, Fail}
    ].

test_float(R0,R1,Fail) ->
    [{'and', R1, R0, ?TAG_PRIMARY_BOXED},
     {jump, {cmp, not_equal, R1, ?TAG_PRIMARY_BOXED}, Fail},
     {'xor', R1, R0, ?TAG_PRIMARY_BOXED},  %% access pointer
     {mov, R0, {mem,R1}},                  %% get header value
     {'and', R0, R0, ?TAG_HEADER_MASK},    %% 
     {jump, {cmp, not_equal, R0, ?TAG_HEADER_FLOAT}, Fail}
    ].

%% after test_float R1 is pointing to the boxed data
load_float(R1,F0) ->
    [{mov_f64, F0, {mem,R1,8}}].

get_float(F0,R0,R1,Fail) ->
    test_float(R0,R1,Fail) ++ 
	load_float(R1,F0).
    
test_tuple(R0,R1,Fail) ->
    [{'and', R1, R0, ?TAG_PRIMARY_BOXED},
     {jump, {cmp, not_equal, R1, ?TAG_PRIMARY_BOXED},Fail},
     {mov, R0, {mem,R1}},                  %% get header value
     {'and', R0, R0, ?TAG_HEADER_MASK},    %% 
     {jump, {cmp, not_equal, R0, ?TAG_HEADER_ARITY}, Fail}
    ].

make_efloat() ->
    [{module,efloat},
     {function, add},
     {enter, [], f64, [term, term], [{reg,1},{freg,1}], [], 0}] ++

     test_float(r0,r2,badarg) ++
     load_float(r2,fr0) ++

     test_float(r1,r2,badarg) ++
     load_float(r2,fr1) ++
     [
     {add_f64, fr0, fr0, fr1},
     {return, mov_f64, fr0},

     {label, badarg},
     {fset64, fr0, 0.0},
     {return, mov_f64, fr0}
    ].

load_efloat() ->
    Compile = sljit:create_compiler(),
    sljit_asm:asm_ins_list(Compile, make_efloat(), #{}),
    sljit:generate_code(Compile).

    
    
