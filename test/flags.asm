%% -*- erlang -*-

{module, flags}.

{function, icomp}.
{enter, [], word, [word,word], [], [{reg,3}], 0}.

{mov, s2, 0}.

{jump, [{not_equal,s0,s1}], l00}.
{'or', s2, s2, 2#0000_0000_0000_0001}.  %% 0
{label, l00}.

{jump, [{equal,s0,s1}], l01}.
{'or', s2, s2, 2#0000_0000_0000_0010}.  %% 0
{label, l01}.

{jump, [{greater_equal,s0,s1}], l1}.
{'or', s2, s2, 2#0000_0000_0000_0100}.  %% 2

{label, l1}.
{jump, [{less,s0,s1}], l2}.
{'or', s2, s2, 2#0000_0000_0000_1000}.  %% 3

{label, l2}.
{jump, [{less_equal,s0,s1}], l3}.
{'or', s2, s2, 2#0000_0000_0001_0000}.  %% 4

{label, l3}.
{jump, [{greater,s0,s1}], l4}.
{'or', s2, s2, 2#0000_0000_0010_0000}.  %% 5

{label, l4}.
{jump, [{sig_greater_equal,s0,s1}], l5}.
{'or', s2, s2, 2#0000_0000_0100_0000}.  %% 6

{label, l5}.
{jump, [{sig_less,s0,s1}], l6}.
{'or', s2, s2, 2#0000_0000_1000_0000}.  %% 7

{label, l6}.
{jump, [{sig_less_equal,s0,s1}], l7}.
{'or', s2, s2, 2#0000_0001_0000_0000}.  %% 8

{label, l7}.
{jump, [{sig_greater,s0,s1}], l8}.
{'or', s2, s2, 2#0000_0010_0000_0000}.  %% 9

{label, l8}.
{sub, [overflow], s0, s1}.
{jump, [not_overflow], l9}.
{'or', s2, s2, 2#0000_0100_0000_0000}.  %% 10

{label, l9}.
{sub, [carry], s0, s1}.
{jump, [not_carry], l10}.
{'or', s2, s2, 2#0001_0000_0000_0000}.  %% 12

{label, l10}.
{mov, r0, s2}.
{return, mov, r0}.


{function, fcomp}.
{enter, [], word, [f64,f64], [{reg,1},{freg,3}], [], 0}.

{mov, r0, 0}.
{jump, [{f_not_equal, fr0, fr1}], lf1}.
{'or', r0, r0, 2#0000_0001_0000_0000_0000_0000}.  %% 16

{label, lf1}.
{jump, [{f_equal, fr0, fr1}], lf2}.
{'or', r0, r0, 2#0000_0010_0000_0000_0000_0000}.  %% 17

{label, lf2}.
{jump, [{f_greater_equal,fr0,fr1}], lf3}.
{'or', r0, r0, 2#0000_0100_0000_0000_0000_0000}.  %% 18

{label, lf3}.
{jump, [{f_less,fr0,fr1}], lf4}.
{'or', r0, r0, 2#0000_1000_0000_0000_0000_0000}.  %% 19

{label, lf4}.
{jump, [{f_less_equal,fr0,fr1}], lf5}.
{'or', r0, r0, 2#0001_0000_0000_0000_0000_0000}.  %% 20

{label, lf5}.
{jump, [{f_greater,fr0,fr1}], lf6}.
{'or', r0, r0, 2#0010_0000_0000_0000_0000_0000}.  %% 21

{label, lf6}.
{return, mov, r0}.
