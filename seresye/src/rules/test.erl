-module(test).
-export([test/2]).
-rules([test]).
test(Engine0,{test,X})  ->
seresye_engine:set_client_state(Engine0,[success|seresye_engine:get_client_state(Engine0)]).