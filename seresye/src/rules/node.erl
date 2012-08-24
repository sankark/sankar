-module(node).
-include("../records/node.hrl").
-include("../records/nodes.hrl").
-include("../records/ontology.hrl").
-include("../records/test.hrl").
-include("../records/wine.hrl").
-export([node/2]).
-rules([node]).
node(Engine0,#node{node_id=Node_id,heap=Heap,memory=Memory,ipaddr=Ipaddr}) when Heap<10 ->
Engine1=seresye_engine:assert(Engine0,{heap_below_10}), 
seresye_engine:set_client_state(Engine1,[heap10|seresye_engine:get_client_state(Engine1)]).