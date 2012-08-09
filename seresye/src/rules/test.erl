-module(test).
-export([test/3]).
-rules([test]).
test(Engine,{test,test},{test2,X}) when X>10 ->
seresye_engine:assert([{test3,test3}]).