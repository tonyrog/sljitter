%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Test 
%%% @end
%%% Created :  2 Feb 2025 by Tony Rogvall <tony@rogvall.se>

-module(float).
-compile(export_all).

compile_and_load() ->
    sljit_asm:assemble("float.asm", "float.bin"),
    sljit_asm:assemble("float.asm", "float.slo"),
    load(),
    ok.

load() ->
    sljit:unregister_code(float),
    {_Xs,_Code} = sljit_asm:load("float.slo"),
    ok.

test(X, Y) ->
    A = sljit:call({float,fadd}, X, Y),
    B = sljit:call({float,fsub}, X, Y),
    sljit:call({float,fmul}, A, B).

    

    
    
