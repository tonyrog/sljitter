%% -*- erlang -*-

{module, test}.
{function, three}.

%% function word add(a) { return a+3; }
%% use 3 registers, save 1 register, no temporaries
{enter, [], word, [word], [{reg,2}], [], 0}.

{mov, r1, 3}.
{add, r0, r0, r1}.

{return, mov, r0}.

%% 0000: endbr64                           f30f1efa
%% 0004: pushq  %rbx                       53
%% 0005: movq  %rdi,%rax                   4889f8
%% 0008: movq  $6612976710612680707,%rsi   48c7c6030000004803c65b
%% 0013: ret                               c3
