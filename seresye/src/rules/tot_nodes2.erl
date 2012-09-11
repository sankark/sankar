-module(tot_nodes2).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([tot_nodes2/3]).
-rules([tot_nodes2]).
tot_nodes2(Engine0,{heap60,Node}=X,{heap60,nodes,A}=Y) when Node#node.heap > 60 ->
		io:format("Node ~n~p",[Y]),
	Engine4=seresye_engine:retract(Engine0,X), 
Engine2=seresye_engine:retract(Engine4,Y), 
Engine3=seresye_engine:assert(Engine2,{heap60,nodes,[Node|A]}), 
seresye_engine:set_client_state(Engine3,[heap60found|seresye_engine:get_client_state(Engine3)]).