%% -*- erlang -*-

{module, test}.
{function, loop}.

%% function word sum(n) { s = 0; for (i = 1; i <= n; i++) s += i; return s; }
%% use 1 registers, save 3 register, no locals
{enter, [], word, [word], [{reg,1}], [{reg,3}], 0}.

%% s0=n
{mov, s1, 1}.   %% s1=i
{mov, s2, 0}.   %% s2=s
{label, again}.
  {jump, [{greater,s1,s0}], done}.
  {add, s2, s2, s1}.
  {add, s1, s1, 1}.
  {jump, again}.
{label, done}.
{mov, r0, s2}. 
{return, mov, r0}.
