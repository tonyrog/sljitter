%% -*- erlang -*-
{module, simd}.
{function, test}.

{enter, [], void, [ptr,ptr,ptr], [{reg,3},{vreg,3}], [], 0}.

{vload, [aligned_128,reg_128], vr0, {mem,r0}}.
{vload, [aligned_128,reg_128], vr1, {mem,r1}}.
{vand, [reg_128], vr2, vr0, vr1}.
{vor, [reg_256], vr2, vr0, vr1}.
{vxor, [reg_128], vr2, vr0, vr1}.
{vstore, [aligned_128,reg_128], vr2, {mem,r2}}.

{return}.
