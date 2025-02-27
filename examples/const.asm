%% -*- erlang -*-

{module, const}.
{function, addival}.
{enter, [], word, [word], [{freg,2}], [], 0}.

{const, ivalue, r1, 12}.

{add, r0, r0, r1}.

{call, [call],  word, [word], {sljit, print_sw}}.

{return, mov, r0}.

	
