-module(scaleup).
-include("../records/proto_template_pb.hrl").
-export([scaleup/3]).
-rules([scaleup]).
scaleup(Engine,{heap60,X},{tot_nodes,Y}) when X > 60/100*Y   ->
seresye_engine:set_client_state(Engine,["scaleup"|seresye_engine:get_client_state(Engine)]).