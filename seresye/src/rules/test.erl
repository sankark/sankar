-module(test).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([test/3]).
-rules([test]).
test(Engine0,{is_heap60,Node}=X,{heap60,nodes,A}=Y)  ->
Engine1=seresye_engine:retract(Engine0,Y), 
Engine2=seresye_engine:retract(Engine1,X), 
Engine3=seresye_engine:assert(Engine2,{heap60,nodes,[Node|A]}), 
seresye_engine:set_client_state(Engine3,[seresye_engine:get_client_state(Engine3)]).