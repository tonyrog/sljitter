%% -*- erlang -*-
%%
%% Test various flavours of jump
%%
%% {jump, [Option], Label}
%% {ijump, [Option], Src}
%% Option = {src,Name} |    -- set name of jump source to change target Label
%%          always | never | Cmp | Status |
%%          {Cmp, S1, S2}
%%
%% Cmp =  equal   |  not_equal |
%%        less    | less_equal |
%%        greater | greater_equal |
%%        sig_less | sig_less_equal |
%%        sig_greater | sig_greater_equal
%%
%% Status = Flag | overflow | not_overflow  | carry | not_carry
%%
{module, jump}.

%% jump version
{function, go}.
{enter, [], word, [], [], [], 0}.

{jump,[{from,jsrc1}],a}.

{label,a}.
{mov, r0, 1}.
{return, mov, r0}.

{label,b}.
{mov, r0, 2}.
{return, mov, r0}.

{label,c}.
{mov, r0, 3}.
{return, mov, r0}.

%% jump indirect version
{function, igo}.
{enter, [], word, [], [], [], 0}.

{ijump, [{from,jsrc2},{dst,r0},{default,f}]}.

{label,d}.
{mov, r0, 4}.
{return, mov, r0}.

{label,e}.
{mov, r0, 5}.
{return, mov, r0}.

{label,f}.
{mov, r0, 6}.
{return, mov, r0}.
