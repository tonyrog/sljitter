%% -*- mode: erlang -*-
{module, test}.
{function, bar}.

{enter, [], word, [word,word], [{reg,1}], [], 0}.

{call, [call], word, [word], {test, foo}}.

{add, r0, r0, r1}.
{return, mov, r0}.
