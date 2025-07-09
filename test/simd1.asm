%% -*- erlang -*-
{module, simd1}.
{function, test}.

{enter, [], void, [ptr,ptr], [{reg,2},{vreg,2}], [], 0}.

{vload, [aligned_128,reg_128], vr0, {mem,r0}}.

{vneg, [reg_128,elem_8], vr1, vr0}.
{vneg, [reg_128,elem_16], vr1, vr0}.
{vneg, [reg_128,elem_32], vr1, vr0}.
{vneg, [reg_128,elem_64], vr1, vr0}.
{vneg, [reg_128,elem_32,float], vr1, vr0}.
{vneg, [reg_128,elem_64,float], vr1, vr0}.

{vnot, [reg_128,elem_8], vr1, vr0}.
{vnot, [reg_128,elem_16], vr1, vr0}.
{vnot, [reg_128,elem_32], vr1, vr0}.
{vnot, [reg_128,elem_64], vr1, vr0}.
{vnot, [reg_128,elem_32,float], vr1, vr0}.
{vnot, [reg_128,elem_64,float], vr1, vr0}.

{vstore, [aligned_128,reg_128], vr1, {mem,r1}}.

{return}.
