%% -*- erlang -*-
{module, simd}.
{function, test}.

{enter, [], void, [ptr,ptr,ptr], [{reg,3},{vreg,3}], [], 0}.

{vload, [aligned_128,reg_128], vr0, {mem,r0}}.
{vload, [aligned_128,reg_128], vr1, {mem,r1}}.

%% {vsll, [reg_128,elem_8],  vr2, vr0, 3}.
{vsll, [reg_128,elem_16], vr2, vr0, 5}.
{vsll, [reg_128,elem_32], vr2, vr0, 7}.
{vsll, [reg_128,elem_64], vr2, vr0, 9}.

%% {vsrl, [reg_128,elem_8],  vr2, vr0, 3}.
{vsrl, [reg_128,elem_16], vr2, vr0, 5}.
{vsrl, [reg_128,elem_32], vr2, vr0, 7}.
{vsrl, [reg_128,elem_64], vr2, vr0, 9}.

%% {vsra, [reg_128,elem_8],  vr2, vr0, 3}.
{vsra, [reg_128,elem_16], vr2, vr0, 5}.
{vsra, [reg_128,elem_32], vr2, vr0, 7}.
%% {vsra, [reg_128,elem_64], vr2, vr0, 9}.

{vstore, [aligned_128,reg_128], vr2, {mem,r2}}.

{return}.
