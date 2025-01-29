%% -*- erlang -*-

%% function word fadd(a, b) { return a+b; }
%% use 0 registers 3 floating, save 0 register, no temporaries
{enter, [], f32, [f32,f32], [{freg,3}], [], 0}.

{add_f32, fr0, fr0, fr1}.

{return, mov_f32, fr0}.

