%% -*- erlang -*-

{module, test}.
{function, sum}.

%% function word sum(byte* ptr, size_t n) { s = 0; while(n--) { s += *ptr; ptr++ } return s; }
%% use 4 registers, 4 saves register, no temporaries
{enter, [], word, [ptr,word], [{reg,4}], [{reg,4}], 0}.
%% s0 = ptr, s1 = n
{mov, s2, 0}.   %% r2 = s
{mov, s3, 0}.   %% r3 = s	
{label, again}.
{jump, [{equal, s1, 0}], done}.
{mov_u8, s3, {mem,s0}}.
{add, s2, s2, s3}.
{add, s0, s0, 1}.
{sub, s1, s1, 1}.	
{jump, again}.
{label, done}.
{mov, r0, s2}. 
{return, mov, r0}.
