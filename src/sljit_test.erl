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
    S0 = 13 + 100 + 200 + 300 + 400,
    S1 = 13 + 500 + 600 + 700 + 800,
    S  = S0+S1,
    S = sljit:call({alloc,add}, 13).


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

access() ->
    access(emulator).
access(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{access,all,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","access.asm"])),
    (1+2+4+8+16) = sljit:call({access, all},
			      <<2:64/native, 
				4:64/native, 
				8:64/native, 
				0:64/native, 
				16:64/native>>, 16),
    ok.

loop() ->
    loop(emulator).

loop(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{test,loop,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","loop.asm"])),
    55 = sljit:call({test,loop}, 10),
    ok.


%% test add of erlang small nums return small num
eadd() ->
    eadd(emulator).
eadd(Backend) ->
    Dir = code:lib_dir(sljitter),
    {[{erlang,add,_}],_Code} = 
	sljit_asm:load(Backend, filename:join([Dir,"examples","eadd.asm"])),
    55 = sljit:call({erlang,add}, 20, 35),
    ok.
