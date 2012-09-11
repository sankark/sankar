-module(tot_heap2).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([tot_heap2/3]).
-rules([tot_heap2]).
tot_heap2(Engine0,{add_heap,#node{}}=F,{tot_heap,X}=G)  ->
Engine1=seresye_engine:retract(Engine0,F), 
Engine2=seresye_engine:retract(Engine1,G), 
Engine3=seresye_engine:assert(Engine2,{tot_heap,X+1}), 
seresye_engine:set_client_state(Engine3,[seresye_engine:get_client_state(Engine3)]).