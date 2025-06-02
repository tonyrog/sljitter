%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Test compare
%%% @end
%%% Created :  1 Jun 2025 by Tony Rogvall <tony@rogvall.se>

-module(compare).

-export([init/0, init/1]).
-export([itest/0, itest/2]).
-export([ftest/0, ftest/2]).
-export([decode_flags/1]).

-include("../include/sljit.hrl").

init() ->
    init(emulator).
init(Arch) ->
    Lib = code:lib_dir(sljitter),
    AsmFile = filename:join([Lib,"test","flags.asm"]),
    {[{flags,icomp,_},{flags,fcomp,_}],_Code} = sljit_asm:load(Arch, AsmFile),
    ok.

mask() ->
    case erlang:system_info(wordsize) of
	4 -> 16#ffffffff;
	8 -> 16#ffffffffffffffff
    end.

mask(X) ->
    X band mask().

itest() ->
    itest(1,1),
    itest(1,2),
    itest(2,1),
    itest(0,1),
    itest(1,0),

    itest(-1,-1),
    itest(-1,-2),
    itest(-2,-1),
    itest(0,-1),
    itest(-1,0),

    itest(1,-1),
    itest(1,-2),
    itest(2,-1),
    itest(0,-1),
    itest(1,0),

    itest(-1,1),
    itest(-1,2),
    itest(-2,1),
    itest(0,1),
    itest(-1,0),
    ok.
    

itest(A, B) ->
    C = sljit:call({flags,icomp}, A, B),
    %% C contains comparison of A and B
    check('<', (C bsr ?SLJIT_LESS) band 1, mask(A) < mask(B)),
    check('>=', (C bsr ?SLJIT_GREATER_EQUAL) band 1, mask(A) >= mask(B)),
    check('>', (C bsr ?SLJIT_GREATER) band 1, mask(A) > mask(B)),
    check('=<', (C bsr ?SLJIT_LESS_EQUAL) band 1, mask(A) =< mask(B)),

    check('<', (C bsr ?SLJIT_SIG_LESS) band 1, A < B),
    check('>=', (C bsr ?SLJIT_SIG_GREATER_EQUAL) band 1, A >= B),
    check('>', (C bsr ?SLJIT_SIG_GREATER) band 1, A > B),
    check('=<', (C bsr ?SLJIT_SIG_LESS_EQUAL) band 1, A =< B),

    check('overflow', (C bsr ?SLJIT_OVERFLOW) band 1, false),
    check('carry', (C bsr ?SLJIT_OVERFLOW) band 1, mask(A-B) < (A-B)),
    ok.


ftest() ->
    ftest(1.0,1.0),
    ftest(1.0,2.0),
    ftest(2.0,1.0),
    ftest(0.0,1.0),
    ftest(1.0,0.0),

    ftest(-1.0,-1.0),
    ftest(-1.0,-2.0),
    ftest(-2.0,-1.0),
    ftest(0.0,-1.0),
    ftest(-1.0,0.0),

    ftest(1.0,-1.0),
    ftest(1.0,-2.0),
    ftest(2.0,-1.0),
    ftest(0.0,-1.0),
    ftest(1.0,0.0),

    ftest(-1.0,1.0),
    ftest(-1.0,2.0),
    ftest(-2.0,1.0),
    ftest(0.0,1.0),
    ftest(-1.0,0.0),
    ok.

ftest(A, B) ->
    C = sljit:call({flags,fcomp}, A, B),
    %% C contains comparison of A and B
    check('==', (C bsr ?SLJIT_F_EQUAL) band 1, A == B),
    check('/=', (C bsr ?SLJIT_F_NOT_EQUAL) band 1, A /= B),

    check('<', (C bsr ?SLJIT_F_LESS) band 1, A < B),
    check('>=', (C bsr ?SLJIT_F_GREATER_EQUAL) band 1, A >= B),
    check('>', (C bsr ?SLJIT_F_GREATER) band 1, A > B),
    check('=<', (C bsr ?SLJIT_F_LESS_EQUAL) band 1, A =< B),

    ok.

check(_Flag, 1, true) -> ok;
check(Flag, 0, true) -> io:format("~s failed\n", [Flag]), error;

check(Flag, 1, false) -> io:format("~s failed\n", [Flag]), error; 
check(_Flag, 0, false) -> ok.

decode_flags(X) ->
    decode_flags(X, 
		 [{'ovf', ?SLJIT_OVERFLOW},
		  {'carry', ?SLJIT_OVERFLOW},
		  {'==',  ?SLJIT_EQUAL},
		  {'/=',  ?SLJIT_NOT_EQUAL},
		  
		  {'u<',  ?SLJIT_LESS},
		  {'u>=', ?SLJIT_GREATER_EQUAL},
		  {'u>',  ?SLJIT_GREATER},
		  {'u=<', ?SLJIT_LESS_EQUAL},
		  
		  {'i<',  ?SLJIT_SIG_LESS},
		  {'i>=', ?SLJIT_SIG_GREATER_EQUAL},
		  {'i>',  ?SLJIT_SIG_GREATER},
		  {'i=<', ?SLJIT_SIG_LESS_EQUAL},
		  
		  {'f==', ?SLJIT_F_EQUAL},
		  {'f/=', ?SLJIT_F_NOT_EQUAL},

		  {'f<',  ?SLJIT_F_LESS},
		  {'f>=', ?SLJIT_F_GREATER_EQUAL},
		  {'f>',  ?SLJIT_F_GREATER},
		  {'f=<', ?SLJIT_F_LESS_EQUAL}], []).

decode_flags(X, [{Name,Bit}|Fs], Acc) ->
    case (X bsr Bit) band 1 of
	1 -> decode_flags(X, Fs, [Name|Acc]);
	0 -> decode_flags(X, Fs, Acc)
    end;
decode_flags(_X, [], Acc) ->
    Acc.
