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

expand_expr_definition(D=#bic_function{body=Body}, Env0) ->
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
		    {X,Stmts,EAcc1} = expand_expr(M, EAcc0),
		    U1 = U#bic_unary{arg=X},
		    Expr1 = Expr#bic_assign{lhs=Lhs,rhs=U1},
		    E1 = E#bic_expr_stmt{expr=Expr1},
		    %% fixme declarations
		    {Stmts++[E1],EAcc1};
		B = #bic_binary{arg1=L,arg2=R} ->
		    {Lx,Stmts1,EAcc1} = expand_expr(L, EAcc0),
		    {Rx,Stmts2,EAcc2} = expand_expr(R, EAcc1),
		    B1 = B#bic_binary{arg1=Lx,arg2=Rx},
		    Expr1 = Expr#bic_assign{lhs=Lhs,rhs=B1},
		    E1 = E#bic_expr_stmt{expr=Expr1},
		    {(Stmts1++Stmts2)++[E1],EAcc2};
		_ ->
		    %% fixme expand
		    {E, EAcc0}
	    end;
	_ ->
	    {E,EAcc0}
    end.

%% create triples
expand_expr(B=#bic_binary{line=Ln,arg1=L,arg2=R},EAcc) ->
    {Lx,Stmts1,EAcc1} = expand_expr(L, EAcc),
    {Rx,Stmts2,EAcc2} = expand_expr(R, EAcc1),
    Xt = combine_type(Lx, Rx),
    {X, EAcc3} = new_id(EAcc2, Ln, Xt),
    A = #bic_assign{op='=',lhs=X,rhs=B#bic_binary{arg1=Lx,arg2=Rx}},
    Stmt = #bic_expr_stmt{line=Ln, expr = A },
    EAcc4 = add_decl(X, EAcc3),
    {X,(Stmts1++Stmts2)++[Stmt], EAcc4};
expand_expr(U=#bic_unary{line=Ln,arg=M}, EAcc) ->
    {Mx,Stmts1,EAcc1} = expand_expr(M,EAcc),
    Xt = combine_type(Mx),
    {X, EAcc2} = new_id(EAcc1, Ln, Xt),
    A = #bic_assign{op='=',lhs=X, rhs=U#bic_unary{arg=Mx}},
    Stmt = #bic_expr_stmt{line=Ln, expr = A },
    {X,Stmts1++[Stmt], EAcc2};
%% fixme add calls assign et etc
expand_expr(X, EAcc) ->
    {X,[],EAcc}.


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
	  Rt=#bic_type { sign=Rs, size=Rz, type= R }) ->
    io:format("Lt = ~p\n", [Lt]),
    io:format("Rt = ~p\n", [Rt]),
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

word_type(Lt=#bic_type{ type=Mt, size = Mz }) ->
    Type = case Mt of
	       char -> int;
	       int -> int;
	       float -> float;
	       double -> double;
	       {T1,T1} -> T1
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
