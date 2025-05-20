%% -*- erlang -*-

{module, alloc}.
{function, add}.

%%{enter, [], word, [word], [{reg,3}], [], 64}. %% 8 64-bit words
{enter, [], word, [word], [{reg,1}], [{reg,3}], 64}. %% 8 64-bit words

{mov, {mem,sp,0},  100}.
{mov, {mem,sp,8},  200}.
{mov, {mem,sp,16}, 300}.
{mov, {mem,sp,24}, 400}.
{mov, {mem,sp,32}, 500}.
{mov, {mem,sp,40}, 600}.
{mov, {mem,sp,48}, 700}.
{mov, {mem,sp,56}, 800}.

{mov, s1, s0}.
{mov, s2, s0}.
{add, s1, s1, {mem,sp,0}}.
{add, s1, s1, {mem,sp,8}}.
{add, s1, s1, {mem,sp,16}}.
{add, s1, s1, {mem,sp,24}}.
{add, s2, s2, {mem,sp,32}}.
{add, s2, s2, {mem,sp,40}}.
{add, s2, s2, {mem,sp,48}}.
{add, s2, s2, {mem,sp,56}}.

{add, s1, s1, s2}.
{mov, r0, s1}.
{return, mov, r0}.
