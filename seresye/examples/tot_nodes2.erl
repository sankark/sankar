-module(tot_nodes2).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([tot_nodes2/3]).
-rules([tot_nodes2]).
tot_nodes2(Engine0,{heap60,#node{node_name=Node_name,heap=Heap}}=M,{heap60,nodes,A}=Y) ->
Engine1=seresye_engine:retract(Engine0,M), 
Engine2=seresye_engine:retract(Engine1,Y), 
Engine3=seresye_engine:assert(Engine2,{heap60,nodes,[A|M]}), 
seresye_engine:set_client_state(Engine3,[heap60found|seresye_engine:get_client_state(Engine3)]).