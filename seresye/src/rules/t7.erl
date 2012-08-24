-module(t7).
-include("../records/node.hrl").
-include("../records/ontology.hrl").
-include("../records/wine.hrl").
-export([t7/2]).
-rules([t7]).
t7(Engine0,#node{node_id=Node_id,heap=Heap,memory=Memory,ipaddr=Ipaddr})  ->
seresye_engine:set_client_state(Engine0,[seresye_engine:get_client_state(Engine0)]).