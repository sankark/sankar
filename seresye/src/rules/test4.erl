-module(test4).
-export([test4/3]).
-rules([test4]).
test4(Engine,{test,test},{test2,X}) when X>10 ->
Engine1=seresye_engine:assert(Engine,[{test3,test3}]), 
seresye_engine:set_client_state(Engine1,[{rule1} | seresye_engine:get_client_state(Engine1)]).