{module, vec}.
{function, vec3add}.

{enter, [], void, [ptr, ptr, ptr], [{freg,3}], [], 0}.

{add_f32, {mem, r2, 0}, {mem, r1, 0}, {mem, r0, 0}}.
{add_f32, {mem, r2, 4}, {mem, r1, 4}, {mem, r0, 4}}.
{add_f32, {mem, r2, 8}, {mem, r1, 8}, {mem, r0, 8}}.

{return}.
