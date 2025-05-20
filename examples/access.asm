%% -*- erlang -*-

{module, access}.
{function, all}.

{enter, [], word, [ptr,word], [{reg,1}], [{reg,2}], 0}.

{mov, r0, 1}.
{'or', r0, r0, s1}.
{'or', r0, r0, {mem,s0}}.
{'or', r0, r0, {mem,s0,8}}.
{'or', r0, r0, {mem,s0,s1}}.
{'or', r0, r0, {mem,s0,s1,1}}.

{return, mov, r0}.
