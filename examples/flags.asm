%% -*- erlang -*-

{module, flags}.
{function, sub}.

%% use 3 registers, save 1 register, no temporaries
{enter, [], word, [word,word], [{reg,5}], [], 0}.

{sub, r2, r0, r1}.  %% subtract r2 = r0-r1
{mov, r2, $_}.
{mov, r3, $_}.
{mov, r4, $_}.
{mov, r5, $_}.
{jump, [not_carry],sub_l1}.
{mov, r2, $C}.
{label,sub_l1}.
{jump, [not_overflow],sub_l2}.
{mov, r3, $V}.
{label,sub_l2}.
{jump, [not_equal],sub_l3}.
{mov, r4, $Z}.
{label,sub_l3}.
%% sig_less N != V
%% sig_greater_equal N == V
{jump, [sig_greater_equal],sub_l4}.
{jump, [overflow],sub_l5}.
  {mov, r5, $N}.
  {jump, sub_l5}.
{label,sub_l4}.
{jump, [not_overflow],sub_l5}.
  {mov, r5, $N}.
{label,sub_l5}.

{mov, r0, r2}.
{shl, r0, r0, 8}.
{add, r0, r0, r3}.
{shl, r0, r0, 8}.
{add, r0, r0, r4}.
{shl, r0, r0, 8}.
{add, r0, r0, r5}.

{return, mov, r0}.

%% check flags after add
{function, add}.

%% use 3 registers, save 1 register, no temporaries
{enter, [], word, [word,word], [{reg,5}], [], 0}.

{add, r2, r0, r1}.  %% add r2 = r0-r1
{mov, r2, $_}.
{mov, r3, $_}.
{mov, r4, $_}.
{mov, r5, $_}.
{jump, [not_carry], add_l1}.
{mov, r2, $C}.
{label,add_l1}.
{jump, [not_overflow], add_l2}.
{mov, r3, $V}.
{label,add_l2}.
{jump, [not_equal], add_l3}.
{mov, r4, $Z}.
{label,add_l3}.
%% sig_less N != V
%% sig_greater_equal N == V
{jump, [sig_greater_equal], add_l4}.
{jump, [overflow], add_l5}.
  {mov, r5, $N}.
  {jump, add_l5}.
{label,add_l4}.
{jump, [not_overflow], add_l5}.
  {mov, r5, $N}.
{label,add_l5}.

{mov, r0, r2}.
{shl, r0, r0, 8}.
{add, r0, r0, r3}.
{shl, r0, r0, 8}.
{add, r0, r0, r4}.
{shl, r0, r0, 8}.
{add, r0, r0, r5}.

{return, mov, r0}.


%% check flags after add
{function, mul}.

%% use 3 registers, save 1 register, no temporaries
{enter, [], word, [word,word], [{reg,5}], [], 0}.

{mul, r2, r0, r1}.  %% mul r2 = r0*r1
{mov, r2, $_}.
{mov, r3, $_}.
{mov, r4, $_}.
{mov, r5, $_}.
{jump, [not_carry], mul_l1}.
{mov, r2, $C}.
{label,mul_l1}.
{jump, [not_overflow], mul_l2}.
{mov, r3, $V}.
{label,mul_l2}.
{jump, [not_equal], mul_l3}.
{mov, r4, $Z}.
{label,mul_l3}.
%% sig_less N != V
%% sig_greater_equal N == V
{jump, [sig_greater_equal], mul_l4}.
{jump, [overflow], mul_l5}.
  {mov, r5, $N}.
  {jump, mul_l5}.
{label,mul_l4}.
{jump, [not_overflow], mul_l5}.
  {mov, r5, $N}.
{label,mul_l5}.

{mov, r0, r2}.
{shl, r0, r0, 8}.
{add, r0, r0, r3}.
{shl, r0, r0, 8}.
{add, r0, r0, r4}.
{shl, r0, r0, 8}.
{add, r0, r0, r5}.

{return, mov, r0}.
