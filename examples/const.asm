%% -*- erlang -*-

{module, const}.
{function, addival}.
{enter, [], word, [word], [], [], 0}.

{const, ivalue, mov, r1, 12}.

{add, r0, r0, r1}.

{call, [call],  word, [word], {sljit, print_sw}}.

{return, mov, r0}.
