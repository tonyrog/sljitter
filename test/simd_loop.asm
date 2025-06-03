%% -*- erlang -*-
{module, simd_loop}.
{function, vor}.

%% function word vor(vec* a, vec* b, vec* c, size_t n) 
%% {
%%    while(n--) {
%%      *c = *a | *b;
%%      a++;
%%      b++;
%%      c++
%%    }
%%  }
%%

{enter, [], void, [ptr,ptr,ptr,word], [{reg,4},{vreg,3}], [], 0}.

{label, loop}.
  {jump, [{equal, r3, 0}], done}.
  {sub, r3, r3, 1}.
  {vload, [aligned_128,reg_128], vr0, {mem,r0}}.
  {vload, [aligned_128,reg_128], vr1, {mem,r1}}.
  {vor, [reg_128], vr2, vr0, vr1}.
  {vstore, [aligned_128,reg_128], vr2, {mem,r2}}.
  {add, r0, r0, vsize}.
  {add, r1, r1, vsize}.
  {add, r2, r2, vsize}.
  {jump, loop}.
{label, done}.
{return}.
