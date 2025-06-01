%% -*- erlang -*-
{module, erlang}.

{function, add}.
{enter, [], term, [term, term], [{reg,1}], [{reg,3}], 0}.

%% when is_small(r0)
{'and', s2, s0, 16#f}.
{jump, [{not_equal, s2, 16#f}], badarg}.

%% when is_small(r1)
{'and', s2, s1, 16#f}.
{jump, [{not_equal, s2, 16#f}], badarg}.

%% arithmetically remove tag from r0 and r1
{'ashr', s0, s0, 4}.
{'ashr', s1, s1, 4}.
%% add r1 to r0 and return
{add, s0, s0, s1}.
{'shl', s0, s0, 4}.
{'or', r0, s0, 16#f}.
{return, mov, r0}.

{label, badarg}.
{mov, r0, 0}.
{return, mov, r0}.
