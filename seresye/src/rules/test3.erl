-module(test3).
-export([test3/3]).
-rules([test3]).
test3(Engine,{test,test},{test2,X}) when X>10 ->
Engine1=seresye_engine:assert(Engine,[{test3,test3}]), 
seresye_engine:set_client_state(Engine1,[{rule1} | seresye_engine:get_client_state(Engine1)]).