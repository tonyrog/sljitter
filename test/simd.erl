%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%     Test some simd code
%%% @end
%%% Created : 11 Jun 2025 by Tony Rogvall <tony@rogvall.se>

-module(simd).

-export([test_vadd/0, test_vadd/1]).

test_vadd() ->
    test_vadd(native).
test_vadd(Arch) ->
    {[{simd_vadd,vadd_u16,4}],_Code} = sljit_asm:load(Arch, "simd_vadd.asm"),
    A16 = << <<X:16/native>> || X <- lists:seq(1,32) >>,
    B16 = << <<X:16/native>> || X <- lists:seq(32,1,-1) >>,
    A = sljit:create_memory(64),
    sljit:write_memory(A, A16),
    B = sljit:create_memory(64),
    sljit:write_memory(B, B16),
    C = sljit:create_memory(64),
    sljit:call({simd_vadd,vadd_u16}, A, B, C, 4),
    CBin = sljit:read_memory(C),
    [ C16 || <<C16:16/native>> <= CBin ].

    
