-module(node_list_init).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([node_list_init/2]).
-rules([node_list_init]).
node_list_init(Engine0,{init})  ->
Engine1=seresye_engine:assert(Engine0,{node_list,[]}), 
seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).