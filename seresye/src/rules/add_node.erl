-module(add_node).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([add_node/3]).
-rules([add_node]).
add_node(Engine0,{add_node,F}=G,{node_list,Nodes}=M)  ->
Engine1=seresye_engine:retract(Engine0,G), 
Engine2=seresye_engine:retract(Engine1,M), 
Engine3=seresye_engine:assert(Engine2,{node_list,[Nodes|F]}), 
seresye_engine:set_client_state(Engine3,[seresye_engine:get_client_state(Engine3)]).