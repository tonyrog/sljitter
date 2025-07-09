%% -*- erlang -*-
{module, simd}.
{function, cmp}.

{enter, [], void, [ptr,ptr,ptr], [{reg,3},{vreg,3}], [], 0}.

{vload, [aligned_128,reg_128], vr1, {mem,r1}}.
{vload, [aligned_128,reg_128], vr2, {mem,r2}}.

{vlt, [reg_128,elem_16],  vr0, vr1, vr2}.
{vgte, [reg_128,elem_16], vr0, vr1, vr2}.
{vgt, [reg_128,elem_16],  vr0, vr1, vr2}.
{vlte, [reg_128,elem_16], vr0, vr1, vr2}.
{veq, [reg_128,elem_16],  vr0, vr1, vr2}.
{vneq, [reg_128,elem_16], vr0, vr1, vr2}.

{vstore, [aligned_128,reg_128], vr2, {mem,r2}}.
{return}.
