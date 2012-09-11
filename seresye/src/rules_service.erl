%% Author: Administrator
%% Created: Aug 9, 2012
%% Description: TODO: Add description to rules_service
-module(rules_service).
-include("records/node.hrl").
-include("records/person.hrl").
-include("records/sample.hrl").
-include("internal.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0,parse_test/1,parse_cons/2,parse_facts/1,test2/1,add_rule/6,test_data/1,parse_term/1 ]).

%%
%% API Functions
%%
-define(line(Tup), element(2, Tup)).
add_rule(RuleName,Pattern,Arity,Cond,Action,State,Salience)->
	io:format("inside Add rule"),
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
    base_engine:add_rule({list_to_atom(filename:basename(Module)),list_to_atom(filename:basename(Module))},99999-list_to_integer(Salience)).
	

%%
%% Local Functions
%%
%parse(X)->
%	{ok,Tokens,_EndLine} = erl_scan:string(X ++ "."),
 %   {ok,AbsForm} = erl_parse:parse_term(Tokens),
%	AbsForm.

add_rule(RuleName,Pattern,Cond,Action,State,Salience)->
	{ok,Tokens,_EndLine} = erl_scan:string(Pattern ++ "."),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
	Arity=length(AbsForm),
	add_rule(RuleName,Pattern,Arity+1,Cond,Action,State,Salience).


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

normalise({'+',_}) -> "+";
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
	%io:format("~nTokens ~p",[Tokens]),
	{ok,Term}=parse_term(Tokens),

	Term.
	%{ok,R1}=erl_parse:parse_exprs(Tokens),
	%Tuples=parse_cons(R1,[]),
	%Res=io:format("~nResult ~p",[Tuples]),
	%io:format("~nResult ~p",[R1]).

to_record({record,Record_Name,Prop_Lists}) when is_atom(Record_Name)->
	 Rec=record_info:set_record(Prop_Lists,record_info:new(Record_Name)),
	 Rec.
	%Rec="{rec_name,[proplists]}",


to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list({rsc_list, L}) -> L;
to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F) -> float_to_list(F).
lib_dir(Dir) ->
	{ok, Path} = application:get_env(seresye,lib_dir),

	filename:join([Path, to_list(Dir)]).

scan() ->	
    Hrl   = filename:join([lib_dir(src),"rules","*.erl"]),
	io:format("~nfiles~p",[Hrl]),
    Files = filelib:wildcard(Hrl) ,
    [ {to_atom(filename:basename(F,".erl")), to_atom(filename:basename(F,".erl"))} ||  F <- Files ].

to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(list_to_binary(L))).

test2({test,Daa})->
	ok.
test_data(Data)->
	
		{A1,A2,A3} = erlang:now(),
	Name=list_to_atom(integer_to_list(A1)++integer_to_list(A2)++integer_to_list(A3)),
%Data="[{heap60,nodes,[]},{records,[{record,node,[{node_name,test},{heap,90}]},{record,node,[{node_name,test3},{heap,50}]},{record,node,[{node_name,test4},{heap,70}]}]}]",
	Facts = parse_facts(Data),

 %io:format("Facts~p",[Facts]),
	{ok,Pid}=worker_engine:start_link(Name),
	 	%Engine=base_engine:get_engine(),
	%io:format("~n New Engine ~p",[Engine]),
	%Join=Engine#seresye.join,
	%Engine2=lists:foldl(fun (Node,Engine1) -> seresye_engine:add_rule (Engine1,Node) end, Engine ,scan()),
	  worker_engine:assert (Name,[{init},Facts]),
Engine10=worker_engine:get_engine(Name),
	%Engine1=seresye_engine:assert(Engine0,Facts),
	  %io:format("~n after assert ~p",[Engine10]),
	Result=seresye_engine:get_client_state(Engine10),
	
	%io:format("~n Client State ~p",[Result]),
	Kb=seresye_engine:get_kb(Engine10),
%base_engine:reset_kb([]),
	%io:format("~n Knowledge Base ~p",[Kb]),
%io:format("~n fired_rule ~p",[Engine10#seresye.fired_rule]),
%io:format("~n hooks ~p",[Engine10#seresye.hooks]),
%io:format("~n join ~p",[Engine10#seresye.join]),
%io:format("~n join ~p",[Engine10#seresye.pending_actions]),
worker_engine:stop(Name),
	{[lists:flatten(io_lib:format("~p", [Name])) || Name <- lists:usort(lists:flatten(Kb))],[lists:flatten(io_lib:format("~p", [Name])) || Name <- lists:usort(lists:flatten(Result))]}.

parse_facts(Data) ->
	   Res = pterm(Data),
	   Records=proplists:get_value(records,Res),
	   Rec=case Records of
		   undefined -> [];
		   _->lists:foldl(fun(T,Acc) -> [to_record(T)|Acc] end, [], Records)
	   end,
	   
	   Facts=lists:flatten([proplists:delete(records, Res)|Rec]),
    Facts.

pterm(Data) ->
    {ok,Tokens,_}=erl_scan:string(Data ++ "."),
 

	   {ok,Res}=rules_service:parse_term(Tokens),
    Res.
test()->
	parse_action("[{assert,{a,\"X+2\"}},{retract,X}]").
%io:format("~p",[Lines]).
	%add_rule("test5","{test,test},{test2,X}=F","X>10","[{assert,{ok}},{assert,{ok}},{retract,F}]","","0").