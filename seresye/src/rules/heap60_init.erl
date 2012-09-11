-module(heap60_init).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([heap60_init/2]).
-rules([heap60_init]).
heap60_init(Engine0,{init})  ->
Engine1=seresye_engine:assert(Engine0,{heap60,nodes,[]}), 
seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).