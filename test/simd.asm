%% -*- erlang -*-
{module, simd}.
{function, test}.

{enter, [], void, [ptr,ptr,ptr], [{reg,3},{vreg,3}], [], 0}.

{vload, [aligned_128,reg_128], vr0, {mem,r0}}.
{vload, [aligned_128,reg_128], vr1, {mem,r1}}.

{vand, [reg_128], vr2, vr0, vr1}.
{vor, [reg_128], vr2, vr0, vr1}.
{vxor, [reg_128], vr2, vr0, vr1}.

{vadd, [reg_128,elem_8], vr2, vr0, vr1}.
{vadd, [reg_128,elem_16], vr2, vr0, vr1}.
{vadd, [reg_128,elem_32], vr2, vr0, vr1}.
{vadd, [reg_128,elem_64], vr2, vr0, vr1}.
{vadd, [reg_128,elem_32,float], vr2, vr0, vr1}.
{vadd, [reg_128,elem_64,float], vr2, vr0, vr1}.

{vsub, [reg_128,elem_8], vr2, vr0, vr1}.
{vsub, [reg_128,elem_16], vr2, vr0, vr1}.
{vsub, [reg_128,elem_32], vr2, vr0, vr1}.
{vsub, [reg_128,elem_64], vr2, vr0, vr1}.
{vsub, [reg_128,elem_32,float], vr2, vr0, vr1}.
{vsub, [reg_128,elem_64,float], vr2, vr0, vr1}.

%%{vmul, [reg_128,elem_8], vr2, vr0, vr1}.  FIXME
{vmul, [reg_128,elem_16], vr2, vr0, vr1}.
{vmul, [reg_128,elem_32], vr2, vr0, vr1}.
%%{vmul, [reg_128,elem_64], vr2, vr0, vr1}.  FIXME
{vmul, [reg_128,elem_32,float], vr2, vr0, vr1}.
{vmul, [reg_128,elem_64,float], vr2, vr0, vr1}.

{vstore, [aligned_128,reg_128], vr2, {mem,r2}}.

{return}.
