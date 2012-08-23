%% Author: Administrator
%% Created: Aug 9, 2012
%% Description: TODO: Add description to rules_service
-module(rules_service).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0,parse_test/1,parse_cons/2,test2/1,add_rule/5,test_data/1,parse_term/1 ]).

%%
%% API Functions
%%
-define(line(Tup), element(2, Tup)).
add_rule(RuleName,Pattern,Arity,Cond,Action,State)->
	Dest="C:/ErlangTools/seresye/src/rules/",
	Header=lists:flatten(io_lib:format("-module(~s).\n~s-export([~s/~w]).\n-rules([~s]).\n",
                                    [RuleName,model_service:get_includes(),
									 RuleName,Arity,
									 RuleName])),
	case length(Cond) of
		X when X>0 -> ConditionLine=lists:flatten(io_lib:format("when ~s",[Cond]));
					_-> ConditionLine=""
		end,
	FunctionLine=lists:flatten(io_lib:format("~s(Engine0,~s) ~s ->\n",[RuleName,
                                     Pattern,
                                     ConditionLine])),

	case length(Action) of
		Y when Y>0 -> {Act,Count}=parse_action(Action),
			ActionLine=lists:flatten(Act);
					_-> ActionLine="",
						Count=0
		end,
	
	case length(State) of
		Z when Z>0 -> StateLine=lists:flatten(io_lib:format("seresye_engine:set_client_state(Engine~p,"
                                     ++ "[~s|seresye_engine:get_client_state(Engine~p)]).",
                                    [Count,State,Count]));
					_-> StateLine=lists:flatten(io_lib:format("seresye_engine:set_client_state(Engine~p,"
                                     ++ "[seresye_engine:get_client_state(Engine~p)]).",
                                    [Count,Count]))
		end,
					
	Module=Dest++RuleName ,
	{ok, RulesFile} = file:open(Module++".erl",[write]),
	io:format(RulesFile,"~s~s~s~s",[Header,FunctionLine,ActionLine,StateLine]),
	file:close(RulesFile),
	rules_compiler:compile_rules(Module),
    seresye:add_rules(defaultengine,list_to_atom(filename:basename(Module))).
	

%%
%% Local Functions
%%
%parse(X)->
%	{ok,Tokens,_EndLine} = erl_scan:string(X ++ "."),
 %   {ok,AbsForm} = erl_parse:parse_term(Tokens),
%	AbsForm.

add_rule(RuleName,Pattern,Cond,Action,State)->
	{ok,Tokens,_EndLine} = erl_scan:string(Pattern ++ "."),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
	Arity=length(AbsForm),
	add_rule(RuleName,Pattern,Arity+1,Cond,Action,State).


parse_cons({cons,1,Tuple1,Rest},Acc)->
	io:format("inside ~p",[Tuple1]),
       parse_cons(Rest,[Acc,Tuple1]);
parse_cons([{cons,1,Tuple1,Rest}],Acc)->
	io:format("inside ~p",[Tuple1]),
       parse_cons(Rest,[Acc,Tuple1]);
parse_cons({nil,1},Acc)->
		lists:flatten(Acc).


parse_term(Tokens) ->
    case erl_parse:parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    try normalise(Expr) of
		Term -> {ok,Term}
	    catch
		_:_R -> {error,{?line(Expr),?MODULE,"bad term"}}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{?line(E2),?MODULE,"bad term"}};
	{error,_} = Err -> Err
    end.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({var,_,S}) -> atom_to_list(S);
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
	error -> erlang:error({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.
build_action([],Count,Lines)->
	{Lines,Count};
build_action([Head|Tail],Count,Lines)->
	case Head of
		{assert,X}->
			 ActionLine=lists:flatten(io_lib:format("Engine~p=seresye_engine:assert(Engine~p,~p), \n",[Count,Count-1,X]));
		{retract,X}->
			ActionLine=lists:flatten(io_lib:format("Engine~p=seresye_engine:retract(Engine~p,~p), \n",[Count,Count-1,X]))
		end,
	build_action(Tail,Count+1,Lines++ActionLine).
parse_action(Action)->
		Term=parse_test(Action++"."),
		{Lines,Count}=build_action(Term,1,""),
		Res=[X || X <- Lines, X =/= $"],
		{Res,Count-1}.
		
parse_test(Action)->
	Test="[{assert,{a,b}},{retract,X},{test,c}].",
	{ok,Tokens,_}=erl_scan:string(Action),
	io:format("~nTokens ~p",[Tokens]),
	{ok,Term}=parse_term(Tokens),
	Term.
	%{ok,R1}=erl_parse:parse_exprs(Tokens),
	%Tuples=parse_cons(R1,[]),
	%Res=io:format("~nResult ~p",[Tuples]),
	%io:format("~nResult ~p",[R1]).

test2({test,Daa})->
	ok.
test_data(Data)->
	{ok,Tokens,_}=erl_scan:string(Data++"."),
	Engine0=seresye:get_engine(defaultengine),
	{ok,Res}=rules_service:parse_term(Tokens),
	 seresye:set_client_state(defaultengine,[]),
	ok=seresye:assert(defaultengine,Res),
	{ok,Result}=seresye:get_client_state(defaultengine),
	io:format("~n Client State ~p",[Result]),
	[lists:flatten(io_lib:format("~s", [Name])) || Name <- Result].
test()->
	%Lines=parse_action("[{assert,{a,b}},{retract,X}]"),
%io:format("~p",[Lines]).
	add_rule("test5","{test,test},{test2,X}=F","X>10","[{assert,{ok}},{assert,{ok}},{retract,F}]","").