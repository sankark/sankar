-module(test).

-export([test_sample/0,init/2,parallel1_test/0,parallel2_test/0,parallel3_test/0,test/2,test2/3]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-rules([init,test]).

-record(agenda, {rules_fired, strategy, rule_list,
                 exec_state, id, pending_actions}).
-record (sample_record, {name, tot}).
test_sample()->

			io:format("Data ~n"),
	Engine=seresye_engine:new([]),
			 E6 = seresye_engine:add_rule (Engine,{test,test},10),
			E7 = seresye_engine:add_rule (E6,{test,test2},0),
			E8 = seresye_engine:add_rule (E7,{test,init},0),
				E10 = seresye_agenda:add_activation(E8,{test,init},[{}],8),
			%E9 = seresye_agenda:add_activation(E8,[{test,init},{test,test}],[{init},{test}],8),
			E2=seresye_engine:assert(E10,[{init},{test,30},{test,10},{init},{test,20},{tot,0}]),
			E1=seresye_agenda:execute_pending(E2),
%% 			%ActName=seresye_agenda:get_activation_from_name(E1,{test,test}),
%% 			ActSal=seresye_agenda:get_activation_from_name(E1,{test,test}),
%% 				io:format("ActName ~p~n",[ActName]),
%% 			io:format("ActSal ~p~n",[ActSal]),
			%E8=seresye_agenda:get_first_activation(E7),
%% 			Engine#seresye{},
%% 	
%% 			 E6 = seresye_engine:add_rule (Engine,{test,init},0),
%% 				
%% 			 E8 = seresye_engine:add_rule (E6,{test,test},1),
%% 			E5=seresye_agenda:add_activation(E8,{test,init},[false],1000),
%% 			E9=seresye_agenda:add_activation(E5,{test,test},[false],100),
%% 			E1=seresye_engine:assert(E9,[{init},{test}]),
			
%% 			%Agenda=seresye_agenda:new(Engine),
%%     Engine0 = seresye_engine:add_rule (Engine,{tot_nodes,tot_nodes},0),
%% 			Engine1 = seresye_engine:add_rule (Engine0,{tot_nodes2,tot_nodes2},0),
%% 			Engine2 = seresye_engine:add_rule (Engine1,{test,init},0),
%% 
%% 			E5=seresye_agenda:get_activation_from_name(Engine0,tot_nodes),
%% 			E6=seresye_engine:assert(Engine2,[]),
 %Engine3=seresye_agenda:set_activation_salience(Engine1,ag1,10),
% Engine4=seresye_agenda:set_activation_salience(Engine2,ag2,11),
 		io:format("E6 ~n~p",[E1]).
	%io:format("E4 ~n~p",[Engine4]).
%% 	Data="[{records,[{record,node,[{node_name,test},{heap,50}]},{record,node,[{node_name,test3},{heap,80}]},{record,node,[{node_name,test4},{heap,70}]}]}]",
%% 		io:format("Data ~n~p",[Data]),
%% 	Facts = rules_service:parse_facts(Data),
%% 		io:format("Facts ~n~p",[Facts]),
%% 	  Engine10 = seresye_engine:assert (Engine2, [{init},Facts]),
%% 	io:format("Engine10 ~n~p",[Engine10]),
%% State = seresye_engine:get_client_state(Engine10),
%% 	io:format("~n~p",[Engine10]),
%% io:format("~n~p",[lists:flatten(State)]).

parallel1_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:process_test_data(Data),
	?debugFmt("~p",[Kb]),
	 ?assertMatch(true,
                 lists:member({init}, Kb)).

parallel2_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:process_test_data(Data),
	?debugFmt("~p",[Kb]),
	 ?assertMatch(true,
                 lists:member({init}, Kb)).
parallel3_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:process_test_data(Data),
	?debugFmt("~p",[Kb]),
	 ?assertMatch(true,
                 lists:member({init}, Kb)).

init(Engine,{init})->
	io:format("Inside init"),
%Engine2 = seresye_engine:retract (Engine, X),
	%Engine1 = seresye_engine:retract (Engine2, N),
	seresye_engine:assert (Engine, {insideinit}).
	

test(Engine,{test,X}=F)  ->
	io:format("Inside test"),
	%E8=seresye_agenda:get_first_activation(Engine),
	%Engine1 = seresye_engine:retract (Engine, Z),
	seresye_engine:assert(Engine,[{temp,X}]).


test2(Engine,{temp,X}=M,{tot,Y}=N)  ->
	io:format("Inside test"),
	%E8=seresye_agenda:get_first_activation(Engine),
	Engine1 = seresye_engine:retract (Engine, M),
	Engine2 = seresye_engine:retract (Engine1, N),
	seresye_engine:assert(Engine2,[{tot,X+Y}]).