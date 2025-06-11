%% -*- erlang -*-
{module, simd_vadd}.

%% function word vadd16(vec_u16* a, vec_u16* b, vec_u16* c, size_t n) 
%% {
%%    while(n--) {
%%      *c = *a + *b;
%%      a++;
%%      b++;
%%      c++
%%    }
%%  }
%%
{function, vadd_u16}.
{enter, [], void, [ptr,ptr,ptr,word], [], [{reg,4}], 48}.

%% save used xmm registers (fixme make aligned?)
  {vstore, [reg_128], vr0, {mem,sp,0}}.
  {vstore, [reg_128], vr1, {mem,sp,16}}.
  {vstore, [reg_128], vr2, {mem,sp,32}}.
{label, loop}.
  {jump, [{equal, s3, 0}], done}.
  {sub, s3, s3, 1}.
  {vload, [reg_128], vr0, {mem,s0}}.
  {vload, [reg_128], vr1, {mem,s1}}.
  {vadd, [reg_128,elem_16], vr2, vr0, vr1}.
  {vstore, [reg_128], vr2, {mem,s2}}.
  {add, s0, s0, vsize}.
  {add, s1, s1, vsize}.
  {add, s2, s2, vsize}.
  {jump, loop}.
{label, done}.
%% restore used xmm registers (fixme. make aligned?)
  {vload, [reg_128], vr0, {mem,sp,0}}.
  {vload, [reg_128], vr1, {mem,sp,16}}.
  {vload, [reg_128], vr2, {mem,sp,32}}.
{return}.
