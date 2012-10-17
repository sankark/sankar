-module(test).
-include("../records/proto_template_pb.hrl").
-export([test/2]).
-rules([test]).
test(Engine,{test})  ->
seresye_engine:set_client_state(Engine,["test"|seresye_engine:get_client_state(Engine)]).