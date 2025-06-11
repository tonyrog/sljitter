%% -*- erlang -*-

{module, float}.

{function, fadd}.
{enter, [], f32, [f32, f32], [{freg,3}], [], 0}.
{add_f32, fr0, fr0, fr1}.
{return, mov_f32, fr0}.

{function, fsub}.
{enter, [], f32, [f32, f32], [{freg,3}], [], 0}.
{sub_f32, fr0, fr0, fr1}.
{return, mov_f32, fr0}.

{function, fmul}.
{enter, [], f32, [f32, f32], [{freg,3}], [], 0}.
{mul_f32, fr0, fr0, fr1}.
{return, mov_f32, fr0}.

{function, fprint}.
{enter, [], word, [f32], [{freg,1}], [], 0}.
{call, [call], word, [f32], {sljit, print_f32}}.
{return, mov, r0}.
