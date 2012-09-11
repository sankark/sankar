-module(test).

-export([test_sample/0,init/2,parallel1_test/0,parallel2_test/0,parallel3_test/0]).

-include_lib("eunit/include/eunit.hrl").
-rules([init]).


test_sample()->

			io:format("Data ~n"),
	Engine=seresye_engine:new([]),
    Engine0 = seresye_engine:add_rule (Engine,{tot_nodes,tot_nodes},0),
			Engine1 = seresye_engine:add_rule (Engine0,{tot_nodes2,tot_nodes2},0),
			Engine2 = seresye_engine:add_rule (Engine1,{test,init},0),
		
	Data="[{records,[{record,node,[{node_name,test},{heap,50}]},{record,node,[{node_name,test3},{heap,80}]},{record,node,[{node_name,test4},{heap,70}]}]}]",
		io:format("Data ~n~p",[Data]),
	Facts = rules_service:parse_facts(Data),
		io:format("Facts ~n~p",[Facts]),
	  Engine10 = seresye_engine:assert (Engine2, [{init},Facts]),
	io:format("Engine10 ~n~p",[Engine10]),
State = seresye_engine:get_client_state(Engine10),
	io:format("~n~p",[Engine10]),
io:format("~n~p",[lists:flatten(State)]).

parallel1_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:test_data(Data),
	?debugMsg("\n"++Kb).

parallel2_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:test_data(Data),
	?debugMsg("\n"++Kb).
parallel3_test()->
	Data="[{records,[{record,node,[{node_name,Test},{heap,70}]},{record,node,[{node_name,Test2},{heap,70}]},{record,node,[{node_name,Test3},{heap,70}]},{record,node,[{node_name,Test4},{heap,70}]}]}]",
	{Kb,C}=rules_service:test_data(Data),
	?debugMsg("\n"++Kb).

init(Engine,{init})->
	
	Engine1=seresye_engine:assert(Engine,[{heap60,nodes,[]}]),
	seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).
	