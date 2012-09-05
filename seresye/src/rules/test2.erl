-module(test2).
-include("../records/node.hrl").
-include("../records/nodes.hrl").
-include("../records/ontology.hrl").
-include("../records/person.hrl").
-include("../records/test.hrl").
-include("../records/wine.hrl").
-export([test2/2]).
-rules([test2]).
test2(Engine0,{hi})  ->
seresye_engine:set_client_state(Engine0,[success|seresye_engine:get_client_state(Engine0)]).