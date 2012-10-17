-module(tot_nodes).
-include("../records/proto_template_pb.hrl").
-export([tot_nodes/2]).
-rules([tot_nodes]).
tot_nodes(Engine,{tot_nodes})  ->
Nodes=accumulate:accum(node,Engine), 
Length=length(Nodes),
seresye_engine:assert(Engine,{tot_nodes,Length}).