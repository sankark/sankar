-module(test).
-include("../records/node.hrl").
-include("../records/nodes.hrl").
-include("../records/person.hrl").
-include("../records/wine.hrl").
-export([test/2]).
-rules([test]).
test(Engine0,{hi})  ->
seresye_engine:set_client_state(Engine0,[success|seresye_engine:get_client_state(Engine0)]).