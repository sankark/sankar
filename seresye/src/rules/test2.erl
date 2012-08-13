-module(test2).
-export([test2/3]).
-rules([test2]).
test2(Engine,{test,test},{test2,X}) when X>10 ->
seresye_engine:assert([{test3,test3}]).