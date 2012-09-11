-module(rule_init).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([rule_init/2]).
-rules([rule_init]).
rule_init(Engine0,{init})  ->
Engine1=seresye_engine:assert(Engine0,{heap60,0}), 
seresye_engine:set_client_state(Engine1,[seresye_engine:get_client_state(Engine1)]).