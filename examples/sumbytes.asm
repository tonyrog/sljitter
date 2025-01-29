%% -*- erlang -*-

{module, test}.
{function, sum}.

%% function word sum(byte* ptr, size_t n) { s = 0; while(n--) { s += *ptr; ptr++ } return s; }
%% use 4 registers, 0 saves register, no temporaries
{enter, [], word, [ptr,word], [{reg,4}], [], 0}.
%% r0 = ptr, r1 = n
{mov, r2, 0}.   %% r2 = s
{mov, r3, 0}.   %% r3 = s	
{label, again}.
{jump, {cmp, equal, r1, 0}, done}.
{mov_u8, r3, {mem,r0}}.
{add, r2, r2, r3}.
{add, r0, r0, 1}.
{sub, r1, r1, 1}.	
{jump, again}.
{label, done}.
{mov, r0, r2}. 
{return, mov, r0}.
