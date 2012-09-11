-module(tot_nodes).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([tot_nodes/2]).
-rules([tot_nodes]).
tot_nodes(Engine0,#node{node_name=Node_name,heap=Heap}=F) when Heap > 60->
Engine1=seresye_engine:assert(Engine0,[{heap60,F}]), 
seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).