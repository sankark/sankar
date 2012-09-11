-module(check_heap60).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([check_heap60/2]).
-rules([check_heap60]).
check_heap60(Engine0,#node{node_name=Node_name,heap=Heap}=F)  ->
Engine1=seresye_engine:assert(Engine0,{is_heap60,F}), 
seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).