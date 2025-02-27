%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    Compile a C like laguage into sljit code
%%% @end
%%% Created : 31 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(sljit_compile).

-export([file/1]).
-compile(export_all).

-include_lib("bic/include/bic.hrl").

%% load environment or setup if native automatic
setup() ->
    ok = appplication:load(sljitter),
    case application:get_env(sljitter, arch, auto) of
	auto ->
	    setup_auto();
	_Arch ->
	    %% assume Arch is setup
	    ok
    end.

%%
%% Setup architecture
%% setup stdint types
%% setup sizeof basic typs
%%
setup_auto() ->
    Ms = macros(),
    arch(Ms,
	 [
	  {{'or',{defined,"__i386__"},{defined,"__i386"}}, 
	   "SLJIT_CONFIG_X86_32"},
	  {{defined, "__x86_64__"}, 
	   "SLJIT_CONFIG_X86_64"},
	  {{defined, "__aarch64__"},
	   "SLJIT_CONFIG_ARM_64"},
	  {{defined,"__thumb2__"}, 
           "SLJIT_CONFIG_ARM_THUMB2"},
	  
	  %% ARM v7
	  {{'and',{defined,"__ARM_ARCH"},{gte,{value,"__ARM_ARCH"},7}},
	   "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_7__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_7A__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_7R__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_7S__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_8A__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_8R__"}, "SLJIT_CONFIG_ARM_V7"},
	  {{defined,"__ARM_ARCH_9A__"}, "SLJIT_CONFIG_ARM_V7"},

	  %% ARM v6
	  {{defined,"__arm__"}, "SLJIT_CONFIG_ARM_V6"},
	  {{defined,"__ARM__"}, "SLJIT_CONFIG_ARM_V6"},

	  %% ppc64
	  {{defined, "__ppc64__"}, "SLJIT_CONFIG_PPC_64"},
	  {{defined, "__powerpc64__"}, "SLJIT_CONFIG_PPC_64"},
	  {{'and', {defined, "_ARCH_PPC64"}, {defined,"__64BIT__"}},
	   "SLJIT_CONFIG_PPC_64"},
	  {{'and', {defined, "_POWER"}, {defined,"__64BIT__"}},
	   "SLJIT_CONFIG_PPC_64"},
	   
	  %% ppc32
	  {{defined,"__ppc__"}, "SLJIT_CONFIG_PPC_32"},
	  {{defined,"__powerpc__"}, "SLJIT_CONFIG_PPC_32"},
	  {{defined,"_ARCH_PPC"}, "SLJIT_CONFIG_PPC_32"},
	  {{defined,"_ARCH_PWR"}, "SLJIT_CONFIG_PPC_32"},
	  {{defined,"_ARCH_PWR2"}, "SLJIT_CONFIG_PPC_32"},
	  {{defined,"_POWER"}, "SLJIT_CONFIG_PPC_32"},

	  %% mips32
	  {{'and', {defined, "__mips__"},{'not',{defined,"_LP64"}}},
	   "SLJIT_CONFIG_MIPS_32"},

	  %% mips64
	  {{defined, "__mips64"}, "SLJIT_CONFIG_MIPS_64"},

	  {{'and', {defined, "__riscv_xlen"}, {eq,{value,"__riscv_xlen"}, 32}},
	   "SLJIT_CONFIG_RISCV_32"},

	  {{'and', {defined, "__riscv_xlen"}, {eq,{value,"__riscv_xlen"}, 64}},
	   "SLJIT_CONFIG_RISCV_64"},


	  {{defined,"__loongarch_lp64"}, "SLJIT_CONFIG_LOONGARCH_64"},

	  {{defined,"__s390x__"}, "SLJIT_CONFIG_S390X"}]).

arch(M, [{Cond, Arch} | CAs]) ->
    case eval(M, Cond) of
	true -> Arch;
	false -> arch(M, CAs)
    end;
arch(_M, []) ->
    false.

eval(M, {defined,Name}) ->
    case maps:find(Name, M) of
	{ok,_} -> true;
	error -> false
    end;
eval(M, {value,Name}) ->
    case maps:find(Name, M) of
	{ok,Value} -> Value;
	error -> false
    end;
eval(_M, Val) when is_number(Val) -> Val;
eval(M, {'and',A,B}) -> eval(M, A) andalso eval(M, B);
eval(M, {'or',A,B}) -> eval(M, A) orelse eval(M, B);
eval(M, {eq, A, B}) -> eval(M,A) =:= eval(M,B);
eval(M, {neq, A, B}) -> eval(M,A) =/= eval(M,B);
eval(M, {lt, A, B}) -> eval(M,A) < eval(M,B);
eval(M, {lte, A, B}) -> eval(M,A) =< eval(M,B);
eval(M, {gt, A, B}) -> eval(M,A) > eval(M,B);
eval(M, {gte, A, B}) -> eval(M,A) >= eval(M,B).


file(F) ->
    case bic:file(F) of
	{ok, Ds} ->
	    io:format("INPUT:\n", []),
	    print_definitions(user, Ds),

	    io:format("UNIQUE:\n", []),
	    Ds1 = bic_unique:definitions(Ds),
	    print_definitions(user, Ds1),

	    Ds2 = move_local_definitions(Ds1),
	    io:format("LOCALS:\n", []),
	    print_definitions(user, Ds2),

	    Ds3 = expand_stmts_definitions(Ds2),
	    io:format("GRAPH:\n\n", []),
	    print_definitions(user, Ds3),

	    Ds4 = expand_expr_definitions(Ds3),
	    io:format("TRIPLETS:\n\n", []),
	    print_definitions(user, Ds4),
	    ok;

	Error ->
	    Error
    end.

macros() ->
    Ms = dump_macros(),
    Ts = [ case re:run(S, "#define ([a-zA-Z0-9_]+) (.*)", [{capture,[1,2],list}]) of
	       {match,[Name,Value]} -> 
		   {Name,to_value(Value)};
	       %% {match,[Name]} -> {Name,""};
	       nomatch -> false
	   end || S <- Ms],
    maps:from_list(lists:filter(fun(false) -> false;
				   (_) -> true end, Ts)).

%% only handle integers are native values rest are strings
to_value(Value) ->
    try list_to_integer(Value) of
	V -> V
    catch
	error:_ -> Value
    end.


%% generate basic stdint types
generate_stdint() ->
    M = macros(),
    maps:fold(
      fun(Name0, Value, Acc) ->
	      case lists:reverse(Name0) of
		  "__EPYT"++Name1 ->
		      "__"++Name2 = lists:reverse(Name1),
		      TypeName = list_to_atom(string:to_lower(Name2)++"_t"),
		      Ts = [list_to_atom(T) || 
			       T <- string:tokens(string:to_lower(Value), " ")],
		      case Ts of
			  [T] ->
			      [make_int(TypeName, undefined, undefined, T)|Acc];
			  [unsigned,T] ->
			      [make_int(TypeName, unsigned, undefined, T)|Acc];
			  [signed,T] ->
			      [make_int(TypeName, signed, undefined, T)|Acc];
			  [Size,T] ->
			      [make_int(TypeName, undefined, Size, T)|Acc];
			  [Size,Sign,T] ->
			      [make_int(TypeName, Sign, Size, T)|Acc]
		      end;
		  _ ->
		      Acc
	      end
      end, [], M).


dump_stdint() ->
    Stdint = generate_stdint(),
    lists:foreach(
      fun(T) ->
	      io:format("~s", [bic_format:definition(T)])
      end, Stdint).


make_int(Name, Sign, Size, Type) ->
    #bic_typedef { line=0, name = Name,
		    type = #bic_type{ sign = Sign,
				      size = Size,
				      type = Type }}.


int_types(Sizeof) ->
    lists:foldl(
      fun(T = {"char",_}, Acc) -> [T | Acc];
	 (T = {"int",_}, Acc) -> [T | Acc];
	 (T = {"short",_}, Acc) -> [T | Acc];
	 (T = {"long",_}, Acc) -> [T | Acc];
	 (T = {"long_long",_}, Acc) -> [T | Acc];
	 (T = {"wchar_t",_}, Acc) -> [T | Acc];
	 (T = {"size_t",_}, Acc) -> [T | Acc];
	 (T = {"int128",_}, Acc) -> [T | Acc];
	 (_, Acc) -> Acc
      end, [], Sizeof).

float_types(Sizeof) ->
    lists:foldl(
      fun(T = {"float",_}, Acc) -> [T | Acc];
	 (T = {"double",_}, Acc) -> [T | Acc];
	 (T = {"long_double",_}, Acc) -> [T | Acc];
	 (T = {"float80",_}, Acc) -> [T | Acc];
	 (T = {"float128",_}, Acc) -> [T | Acc];
	 (_, Acc) -> Acc
      end, [], Sizeof).

generate_sizeof() ->
    generate_sizeof(macros()).

generate_sizeof(M) ->
    maps:fold(
      fun ("__SIZEOF_"++Name0, Value, Acc) ->
	      Name = case lists:reverse(Name0) of
			 "__"++Name1 ->
			     lists:reverse(Name1);
			 _ -> Name0
		     end,
	      [{list_to_atom(string:to_lower(Name)), Value} | Acc];
	  (_, _, Acc) ->
	      Acc
      end, [], M).

sizeof(Type) ->
    sizeof(Type, generate_sizeof()).

sizeof(Type, Sizeof) when is_list(Sizeof) ->
    sizeof(Type, maps:from_list(Sizeof));
sizeof(#bic_typedef{type=Type}, Sizeof) when is_map(Sizeof) ->
    sizeof(Type, Sizeof);
sizeof(#bic_type{size=Size,type=Type}, Sizeof) when is_map(Sizeof) ->
    case Type of
	char -> 1;
	float -> maps:get(float, Sizeof);
	double -> maps:get(double, Sizeof);
	int when Size =:= undefined -> maps:get(int, Sizeof);
	int -> maps:get(Size, Sizeof);
	#bic_pointer{} -> maps:get(pointer,Sizeof)
    end.

%% filter out __SIZE_OF macros"
dump_sizeof() ->
    Ms = dump_macros(),
    lists:filter(fun(S) -> string:find(S, "SIZEOF") =/= nomatch end, Ms).

dump_macros() ->
    Data = os:cmd("echo | gcc -dM -E -"),
    string:split(Data, "\n", all).

print_definitions(Fd, Ds) ->
    Data = bic_format:definitions(Ds),
    io:put_chars(Fd, Data).

%% expand definitions
expand_definitions(Ds) ->
    bic_unique:definitions(Ds).

%%
%% transform c code into callgraph
%% remove loops/whie/if/switch 
%% keep label if-goto and goto's
%%

%% move all local variables to the top of the function
move_local_definitions([D|Ds]) ->
    D1 = move_local_definition(D),
    [D1 | move_local_definitions(Ds)];
move_local_definitions([]) ->
    [].

move_local_definition(D=#bic_function{body=Body}) ->
    {Body1, Locals} = 
	bic_transform:fold_list(
	  fun(Decl=#bic_decl{line=L}, Acc) ->
		  case Decl#bic_decl.value of 
		      undefined ->
			  {false, [Decl|Acc]};
		      Value ->
			  Decl1 = Decl#bic_decl{value=undefined},
			  ID = #bic_id{line=Decl#bic_decl.line,
				       type=Decl#bic_decl.type,
				       name=Decl#bic_decl.name},
			  Expr = #bic_assign{line=L,
					     type=Decl#bic_decl.type,
					     op = '=',
					     lhs=ID,
					     rhs=Value},
			  ExprStmt = #bic_expr_stmt{line=L,expr=Expr},
			  {ExprStmt, [Decl1|Acc]}
		  end;
	     (Stmt, Acc) ->
		  {Stmt, Acc}
	  end, [], Body),
    D#bic_function{body=lists:reverse(Locals,Body1)};
move_local_definition(D) ->
    D.

expand_stmts_definitions([D|Ds]) ->
    D1 = expand_stmts_definition(D),
    [D1 | expand_stmts_definitions(Ds)];
expand_stmts_definitions([]) ->
    [].

expand_stmts_definition(D=#bic_function{body=Body}) ->
    Body1 = expand_stmts(Body),
    D#bic_function{body=Body1};
expand_stmts_definition(D) ->
    D.


%% move all local variables to the top of the function
expand_expr_definitions([D|Ds]) ->
    Env = init_env(),
    D1 = expand_expr_definition(D, Env),
    [D1 | expand_expr_definitions(Ds)];
expand_expr_definitions([]) ->
    [].


%% expand
%%    x = a+b+c =>
%%    x' has common type?
%%    x' = a+b
%%    x = x'+c

expand_expr_definition(D=#bic_function{name=_Name,body=Body}, Env0) ->
    io:format("FUNCTION ~s\n", [_Name]),
    io:format("BODY ~p\n", [Body]),
    {Body1, {Locals,_Env}} = 
	bic_transform:fold_list(
	  fun(Decl=#bic_decl{}, {Locals,Env}) ->
		  Env1 = Env#{ Decl#bic_decl.name => Decl#bic_decl.type },
		  {Decl, {Locals, Env1}};
	     (A=#bic_expr_stmt{}, EAcc) ->
		  expand_expr_stmt(A, EAcc);
	     (Stmt, EAcc) ->
		  {Stmt, EAcc}
	  end, {[], Env0}, Body),
    D#bic_function{body=lists:reverse(Locals,Body1)};
expand_expr_definition(D, _Env0) ->
    D.

expand_expr_stmt(E=#bic_expr_stmt{expr=Expr},EAcc0) ->
    case Expr of
	#bic_assign{op='=',lhs=Lhs,rhs=Rhs} ->
	    case Rhs of
		U = #bic_unary{arg=M} ->
		    {X,Pre,Post,EAcc1} = expand_expr(M, EAcc0),
		    U1 = U#bic_unary{arg=X},
		    Expr1 = Expr#bic_assign{lhs=Lhs,rhs=U1},
		    E1 = E#bic_expr_stmt{expr=Expr1},
		    %% fixme declarations
		    {Pre++[E1]++Post,EAcc1};
		B = #bic_binary{arg1=L,arg2=R} ->
		    {Lx,Pre1,Post1,EAcc1} = expand_expr(L, EAcc0),
		    {Rx,Pre2,Post2,EAcc2} = expand_expr(R, EAcc1),
		    B1 = B#bic_binary{arg1=Lx,arg2=Rx},
		    Expr1 = Expr#bic_assign{lhs=Lhs,rhs=B1},
		    E1 = E#bic_expr_stmt{expr=Expr1},
		    {(Pre1++Pre2)++[E1]++(Post1++Post2),EAcc2};
		_ ->
		    %% fixme expand
		    {E, EAcc0}
	    end;
	Expr ->
	    {_Lx,Pre,Post,EAcc1} = expand_expr(Expr, EAcc0),
	    {Pre++Post, EAcc1};
	_ ->
	    {E,EAcc0}
    end.

%% create triples
expand_expr(B=#bic_binary{op=cast,line=Ln,arg1=T,arg2=E},EAcc) ->
    {Ex,Pre1,Post1,EAcc1} = expand_expr(E, EAcc),
    {X, EAcc2} = new_id(EAcc1, Ln, T),
    A = #bic_assign{op='=',lhs=X,rhs=B#bic_binary{arg1=T,arg2=Ex}},
    Stmt = #bic_expr_stmt{line=Ln, expr = A },
    EAcc3 = add_decl(X, EAcc2),
    {X,(Pre1)++[Stmt], Post1, EAcc3};
expand_expr(B=#bic_binary{line=Ln,arg1=L,arg2=R},EAcc) ->
    {Lx,Pre1,Post1,EAcc1} = expand_expr(L, EAcc),
    {Rx,Pre2,Post2,EAcc2} = expand_expr(R, EAcc1),
    Xt = combine_type(Lx, Rx),
    {X, EAcc3} = new_id(EAcc2, Ln, Xt),
    A = #bic_assign{op='=',lhs=X,rhs=B#bic_binary{arg1=Lx,arg2=Rx}},
    Stmt = #bic_expr_stmt{line=Ln, expr = A },
    EAcc4 = add_decl(X, EAcc3),
    {X,Pre1++Pre2++[Stmt],Post1++Post2, EAcc4};
expand_expr(U=#bic_unary{op=Op,line=Ln,arg=M}, EAcc) ->
    io:format("U = ~p\n", [U]),
    {Mx,Pre1,Post1,EAcc1} = expand_expr(M,EAcc),
    Xt = combine_type(Mx),
    {X, EAcc2} = new_id(EAcc1, Ln, Xt),
    {Pre,Post} =
	case Op of
	    '++' ->
		One = #bic_constant{line=Ln,base=10,value=1,token="1"},
		A = #bic_assign{line=Ln,op='=',lhs=X,rhs=
				    #bic_binary{op='+',arg1=X,arg2=One}},
		Stmt = #bic_expr_stmt{line=Ln, expr = A },
		{Pre1++[Stmt], Post1};
	    '+++' ->
		One = #bic_constant{line=Ln,base=10,value=1,token="1"},
		A = #bic_assign{line=Ln,op='=',lhs=X,rhs=
				    #bic_binary{op='+',arg1=X,arg2=One}},
		Stmt = #bic_expr_stmt{line=Ln, expr = A },
		{Pre1, [Stmt]++Post1};
	    _ ->
		A = #bic_assign{op='=',lhs=X, rhs=U#bic_unary{arg=Mx}},
		Stmt = #bic_expr_stmt{line=Ln, expr = A },
		{Pre1,Post1++[Stmt]}
	end,
    {X,Pre,Post,EAcc2};
%% fixme add calls assign et etc
expand_expr(X, EAcc) ->
    {X,[],[],EAcc}.


add_decl(#bic_id { name = Name, type = Type }, {Local,Env}) ->
    Decl = #bic_decl{ name = Name, type = Type},
    {[Decl|Local], Env}.

%%
%% Combine type of Lx and Rx into X!
%% and generate a declaraion of X
%%
combine_type(Lx, Rx) ->
    Lt = case Lx of
	     #bic_id { type = T1 } ->
		 T1;
	     #bic_constant { type = T1 } ->
		 T1
	 end,
    Rt = case Rx of
	     #bic_id { type = T2 } ->
		 T2;
	     #bic_constant { type = T2 } ->
		 T2
	 end,
    word_type(Lt, Rt).

combine_type(Mx) ->
    Mt = case Mx of
	     #bic_id { type = T1 } ->
		 T1;
	     #bic_constant { type = T1 } ->
		 T1
	 end,
    word_type(Mt).

word_type(Lt=#bic_type{ sign=Ls, size=Lz, type=L}, 
	  _Rt=#bic_type { sign=Rs, size=Rz, type=R }) ->
    Type = case {L, R} of
	       {char,char} -> int;
	       {int,char} -> int;
	       {char,int} -> int;
	       {int,int} -> int;
	       {float,float} -> float;
	       {double,float} -> double;
	       {float,double} -> double;
	       {double,double} -> double;
	       {T1,T1} -> T1
	   end,
    Size = if Lz =:= long_long; Rz =:= long_long -> long_long;
	      Lz =:= short; Rz =:= short -> long;
	      Lz =:= long; Rz =:= long -> long;
	      Lz =:= undefined; Rz =:= undefined -> long
	   end,
    Sign = if Ls =:= signed; Rs =:= signed -> signed;
	      Ls =:= unsigned; Rs =:= unsigned -> unsigned;
	      true -> unsigned
	   end,
    Lt#bic_type { size = Size, type = Type, sign = Sign }.

word_type(Lt=#bic_pointer{ type=_Pt }) ->
    Lt;
word_type(Lt=#bic_type{ type=Mt, size = Mz }) ->
    Type = case Mt of
	       char -> int;
	       int -> int;
	       float -> float;
	       double -> double;
	       T1 -> T1
	   end,
    Size = if Mz =:= long_long -> long_long;
	      Mz =:= short -> long;
	      Mz =:= long -> long;
	      Mz =:= undefined -> long
	   end,
    Lt#bic_type { type = Type, size = Size }.


new_id({Locals,Env}, Ln, Type) ->
    {X, Env1} = create_var(Env, Ln, Type),
    {X, {Locals,Env1}}.

%%
%% environment:
%%   '#label':  next label number to use
%%

init_env() ->
    #{ '#var' => 1, '#label' => 1 }.

-define(LABEL_PREFIX, "LL").
-define(VAR_PREFIX, "VV").

create_label(Env, Ln) ->
    L = maps:get('#label', Env),
    Name = ?LABEL_PREFIX ++ integer_to_list(L),
    ID = #bic_id{line=Ln, type=undefined, name=Name },
    %% Jump
    Goto = #bic_goto { line=Ln, label=ID },
    Empty = undefined, %% #bic_empty { line=Ln },
    %% destination
    Label = #bic_label{ line=Ln, name=Name,code=Empty },
    {Label, Goto, Env#{ '#label' => L+1 }}.

create_var(Env, Ln, Type) ->
    V = maps:get('#var', Env),
    Name = ?VAR_PREFIX ++ integer_to_list(V),
    ID = #bic_id{line=Ln, type=Type, name=Name},
    {ID, Env#{ '#var' => V+1 }}.

expand_stmts(Stmts) ->
    Env0 = init_env(),
    {Body, _Env} = expand_stmts_(Stmts, Env0),
    Body.

expand_stmts_(Stmts, Env0) ->
    bic_transform:fold_list(
      fun(A=#bic_assign{op=Op}, Env) ->
	      case Op of
		  '*=' -> {assign('*', A), Env};
		  '/=' -> {assign('/', A), Env};
		  '%=' -> {assign('%', A), Env};
		  '+=' -> {assign('+', A), Env};
		  '-=' -> {assign('-', A), Env};
		  '<<=' -> {assign('<<', A), Env};
		  '>>=' -> {assign('>>', A), Env};
		  '&=' -> {assign('&', A), Env};
		  '^=' -> {assign('^', A), Env};
		  '|=' -> {assign('|', A), Env};
		  '=' -> {A, Env}
	      end;
	(#bic_if {line=Ln,test=Test,then=Then,'else'=undefined}, Env) ->
	      {Label, Goto, Env1} = create_label(Env, Ln),
	      Not = #bic_unary { line=Ln, op='!', arg=Test},
	      Stmts1 =
		  [#bic_if{line=Ln,test=Not,then=Goto}]++
		  body_list(Then) ++
		  [Label],
	      {Stmts1, Env1};

	 (#bic_if {line=Ln,test=Test,then=Then,'else'=Else}, Env) ->
	      {Label1, Goto1, Env1} = create_label(Env, Ln),
	      {Label2, Goto2, Env2} = create_label(Env1, Ln),
	      Not = #bic_unary { line=Ln, op='!', arg=Test},
	      Stmts1 = [#bic_if{line=Ln,test=Not,then=Goto1}]++
		  body_list(Then) ++
		  [Goto2,
		   Label1] ++
		  body_list(Else) ++
		  [Label2],
	      {Stmts1, Env2};

	 (#bic_while {line=Ln,test=Test,body=Body}, Env) ->
	      {Label1, Goto1, Env1} = create_label(Env, Ln),
	      {Label2, Goto2, Env2} = create_label(Env1, Ln),
	      Not = #bic_unary { line=Ln, op='!', arg=Test},
	      Stmts1 =
		  [Label1,#bic_if{line=Ln,test=Not,then=Goto2}] ++
		  body_list(Body) ++
		  [ Goto1, Label2 ],
	      {Stmts1, Env2};

	 (#bic_do {line=Ln,test=Test,body=Body}, Env) ->
	      {Label1, Goto1, Env1} = create_label(Env, Ln),
	      Stmts1 = [Label1] ++
		  body_list(Body) ++
		  [#bic_if{line=Ln,test=Test,then=Goto1}],
	      {Stmts1, Env1};

	 (#bic_for {line=Ln,init=Init,test=Test,update=Update,body=Body},Env) ->
	      {Label1, Goto1, Env1} = create_label(Env, Ln),
	      {Label2, Goto2, Env2} = create_label(Env1, Ln),
	      Stmts1 = [Init,Label1,#bic_if{line=Ln,test=Test,then=Goto2}] ++
		  body_list(Body) ++
		  [Update,Goto1,Label2],
	      {Stmts1,Env2};

	 (Stmt, Env) ->
	      {Stmt, Env}
      end, Env0, Stmts).


body_list(#bic_compound{code=Cs}) -> Cs;
body_list(Stmt) -> [Stmt].

%% rewrite asignment update into asignment 
assign(Op, A = #bic_assign{line=Ln, type=T, lhs=Lhs, rhs=Rhs}) ->
    Rhs1 = #bic_binary{line=Ln,op=Op,type=T, arg1=Lhs, arg2=Rhs},
    A#bic_assign{op='=',lhs=Lhs,rhs=Rhs1}.
