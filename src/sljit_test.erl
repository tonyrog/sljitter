%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%        Test functions
%%% @end
%%% Created : 18 May 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit_test).

-compile(export_all).

alloc() ->
    alloc(emulator).
alloc(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{alloc,add,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","alloc.asm"])),
    sljit:call({alloc,add}, 13).


sum() ->
    sum(emulator).
sum(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{test,sum,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","sumbytes.asm"])),
    Ptr = <<1,2,3,4,5,6>>,
    sljit:call({test,sum}, Ptr, byte_size(Ptr)).

two() ->
    two(emulator).
two(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{test,_F,_},{test,_G,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","test.asm"])),
    13 = sljit:call({test,three}, 10),
    36 =  sljit:call({test,two}, 10),
    ok.
