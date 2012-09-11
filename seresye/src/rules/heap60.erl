-module(heap60).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([heap60/2]).
-rules([heap60]).
heap60(Engine0,{heap60,X}) when X>3 ->
seresye_engine:set_client_state(Engine0,[success|seresye_engine:get_client_state(Engine0)]).