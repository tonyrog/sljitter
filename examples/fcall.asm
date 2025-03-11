%% -*- erlang -*-
%% test fast call mode

{module, fcall}.
{function, entry}.
{enter, [], word, [], [], [{reg,1}], 8}.

{mov, {mem,sp,0}, 0}.  %% local[0] = 0
{ijump, [fast_call,{dst,s0},{default,a}]}.
{ijump, [fast_call,{dst,s0},{default,add}]}.

{ijump, [fast_call,{dst,s0},{default,b}]}.
{ijump, [fast_call,{dst,s0},{default,add}]}.

{ijump, [fast_call,{dst,s0},{default,c}]}.
{ijump, [fast_call,{dst,s0},{default,add}]}.

{mov, r0, {mem,sp,0}}.
{return, mov, r0}.

{label,a}.
{fast_enter, s0}.
{mov, r0, 1}.
{call, [call],  word, [word], {sljit, print_sw}}.
{call, [call],  word, [], {sljit, print_ln}}.
{mov, r0, 1}.
{fast_return, s0}.

{label,b}.
{fast_enter, s0}.
{mov, r0, 2}.
{call, [call],  word, [word], {sljit, print_sw}}.
{call, [call],  word, [], {sljit, print_ln}}.
{mov, r0, 2}.
{fast_return, s0}.

{label,c}.
	
{fast_enter, s0}.
{mov, r0, 3}.
{call, [call],  word, [word], {sljit, print_sw}}.
{call, [call],  word, [], {sljit, print_ln}}.
{mov, r0, 3}.
{fast_return, s0}.


{label,add}.
{fast_enter, s0}.
{add, {mem,sp,0}, {mem,sp,0}, r0}.
{fast_return, s0}.
