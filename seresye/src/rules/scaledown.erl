-module(scaledown).
-include("../records/proto_template_pb.hrl").
-export([scaledown/3]).
-rules([scaledown]).
scaledown(Engine,{heap60,X},{tot_nodes,Y}) when (X < 60/100*Y) and (X /= 0) ->
seresye_engine:set_client_state(Engine,["scaledown"|seresye_engine:get_client_state(Engine)]).