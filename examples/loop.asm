%% -*- erlang -*-

{module, test}.
{function, loop}.

%% function word sum(n) { s = 0; for (i = 1; i <= n; i++) s += i; }
%% use 3 registers, save 2 register, no temporaries
{enter, [], word, [word], [{reg,3}], [{reg,2}], 0}.

{mov, r1, 1}.   %% r1 = i
{mov, r2, 0}.   %% r2 = s
{label, again}.
{jump, {cmp, greater, r1, r0}, done}.
{add, r2, r2, r1}.
{add, r1, r1, 1}.
{jump, again}.
{label, done}.
{mov, r0, r2}. 
{return, mov, r0}.

%% 0000: endbr64                           f30f1efa
%% 0004: pushq  %rbx                       53
%% 0005: pushq  %rdi                       4157
%% 0007: movq  %rdi,%rax                   4889f8
%% 000a: subq  $8,%rsp                     4883ec08
%% 000e: movq  $56232632416993281,%rsi     48c7c60100000048c7c700
%% 0019: addb  %al                         0000
%% 001b: addb  %cl                         0048
%% 001d: cmpl  %eax,%esi                   3bf0
%% 001f: ja  9                             7709
%% 0021: addq  %rsi,%rdi                   4803fe
%% 0024: addq  $1,%rsi                     4883c601
%% 0028: jmp  -14                          ebf2
%% 002a: movq  %rdi,%rax                   4889f8
%% 002d: addq  $8,%rsp                     4883c408
%% 0031: popq  %rdi                        415f
%% 0033: popq  %rbx                        5b
%% 0034: ret                               c3
