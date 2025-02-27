%% -*- erlang -*.
{module, erlang}.

{function, add}.
{enter, [], word, [term, term], [{reg,1}], [], 0}.

%% when is_small(r0)
{'and', r2, r0, 16#f}.
{jump, {cmp, not_equal, r2, 16#f}, badarg}.

%% when is_small(r1)
{'and', r2, r1, 16#f}.
{jump, {cmp, not_equal, r2, 16#f}, badarg}.

%% arithmetically remove tag from r0 and r1
{'ashr', r0, r0, 4}.
{'ashr', r1, r1, 4}.
%% add r1 to r0 and return
{'add', r0, r0, r1}.
{return, mov, r0}.

{label, badarg}.
{mov, r0, 0}.
{return, mov, r0}.
