%% -*- erlang -*-

{module, access}.
{function, all}.

{enter, [], void, [word,word,word], [{reg,3}], [], 0}.

{mov, r0, 10}.
{mov, r0, r1}.
{mov, r0, {mem,12}}.
{mov, r0, {mem,r1}}.
{mov, r0, {mem,r1,12}}.
{mov, r0, {mem,r1,r2}}.
{mov, r0, {mem,r1,r2,2}}.

{return, mov, r0}.
