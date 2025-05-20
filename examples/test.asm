%% -*- erlang -*-

{module, test}.


%% function word three(a) { return a+3; }
%% use 2 registers, save 0 register, no temporaries
{function, three}.
{enter, [], word, [word], [{reg,2}], [], 0}.
  {mov, r1, 3}.
  {add, r0, r0, r1}.
{return, mov, r0}.


%% function word two(x) { return x+three(x)+three(x+1); }
%% use 2 registers, save 1 register, no temporaries
{function, two}.
{enter, [], word, [word], [{reg,2}], [{reg,2}], 0}.
  {mov, s1, s0}.   %% x
  {mov, r0, s1}.
  {call, [call],  word, [word], {sljit, print_sw}}.
  {call, [call],  word, [], {sljit, print_ln}}.
  
  {mov, r0, s0}.   %% ro=x
  {call, [call], word, [word], three}.
  {add, s1, s1, r0}.
  {mov, r0, s1}.
  {call, [call],  word, [word], {sljit, print_sw}}.
  {call, [call],  word, [], {sljit, print_ln}}.

  {mov, r0, s0}.   %% ro=x
  {call, [call], word, [word], three}.
  {add, s1, s1, r0}.

  {mov, r0, s1}.
  {call, [call],  word, [word], {sljit, print_sw}}.
  {call, [call],  word, [], {sljit, print_ln}}.

  {mov, r0, s1}.
{return, mov, r0}.




