%% -*- erlang -*-
{module, test}.
{function, func_call}.

{enter, [], word, [word, word, word], [{reg,3}], [{reg,3}], 0}.

{'and', r0, s0, 1}.

{jump, [{equal, r0, 0}], print_c}.
%% print b
{mov, r0, s1}.
{icall, [call], word, [word], 16#12345678}.
{jump, out}.
{label, print_c}.
{mov, r0, s2}.
{icall, [call], word, [word], 16#12345678}.
{label, out}.
{return, mov, r0}.
