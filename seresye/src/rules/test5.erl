-module(test5).
-export([test5/3]).
-rules([test5]).
test5(Engine0,{test,test},{test2,X}=F) when X>10 ->
Engine1=seresye_engine:assert(Engine0,{ok}), 
Engine2=seresye_engine:assert(Engine1,{ok}), 
Engine3=seresye_engine:retract(Engine2,F), 
seresye_engine:set_client_state(Engine3,[seresye_engine:get_client_state(Engine3)]).