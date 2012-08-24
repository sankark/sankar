-module(test6).
-include("../records/ontology.hrl").
-include("../records/wine.hrl").
-export([test6/2]).
-rules([test6]).
test6(Engine0,#wine{name=Name,color=Color,flavor=Flavor,grape=Grape,sugar=Sugar})  ->
seresye_engine:set_client_state(Engine0,[seresye_engine:get_client_state(Engine0)]).