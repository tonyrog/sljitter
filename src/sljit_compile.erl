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
	    io:format("INPUT:\n~p\n", [Ds]),
	    Ds1 = move_local_definitions(Ds),
	    io:format("LOCALS:\n~p\n", [Ds1]),
	    Ds2 = expand_stmts_definitions(Ds1),
	    io:format("GRAPH:\n~p\n", [Ds2]),
	    print_definitions(user, Ds2),
	    Ds2;
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
    D1 = bic_unique:definition(D),
    D2 = move_local_definition(D1),
    [D2 | move_local_definitions(Ds)];
move_local_definitions([]) ->
    [].

move_local_definition(D=#bic_function{body=Body}) ->
    {Body1, Locals} = 
	bic_transform:fold_list(
	  fun(Decl=#bic_decl{line=L}, Acc) ->
		  case Decl#bic_decl.value of 
		      undefined ->
			  %% maybe fix that we can return false
			  %% to have the fold delete the element?
			  %% now we may have to clean up empty statements
			  {#bic_empty{line=L}, [Decl|Acc]};
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
			  ExprStmt = #bic_expr_stmt{expr=Expr,line=L},
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

%%
%% environment:
%%   '#label':  next label number to use
%%

init_env() ->
    #{ '#label' => 1 }.

-define(LABEL_PREFIX, "LL").

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
	      Stmts1 = [#bic_if{line=Ln,test=Not,then=Goto},
		       Then,
		       Label
		      ],
	      {Stmts1, Env1};

	 (#bic_if {line=Ln,test=Test,then=Then,'else'=Else}, Env) ->
	      {Label1, Goto1, Env1} = create_label(Env, Ln),
	      {Label2, Goto2, Env2} = create_label(Env1, Ln),
	      Not = #bic_unary { line=Ln, op='!', arg=Test},
	      Stmts1 = [#bic_if{line=Ln,test=Not,then=Goto1},
		       Then,
		       Goto2,
		       Label1,
		       Else,
		       Label2
		      ],
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
