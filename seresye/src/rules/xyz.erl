-module(xyz).
-include("../records/node.hrl").
-include("../records/nodes.hrl").
-include("../records/ontology.hrl").
-include("../records/test.hrl").
-include("../records/wine.hrl").
-export([xyz/2]).
-rules([xyz]).
xyz(Engine0,#node{node_id=Node_id,heap=Heap,memory=Memory,ipaddr=Ipaddr}) when Heap>10 ->
Engine1=seresye_engine:assert(Engine0,{success}), 
seresye_engine:set_client_state(Engine1,[success|seresye_engine:get_client_state(Engine1)]).