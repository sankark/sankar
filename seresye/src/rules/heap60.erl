-module(heap60).
-include("../records/proto_template_pb.hrl").
-export([heap60/2]).
-rules([heap60]).
heap60(Engine,{heap60})  ->
Heap60Nodes=accumulate:accum(node,Engine,[{heap,gt,60}]), 
Length=length(Heap60Nodes),
seresye_engine:assert(Engine,{heap60,Length}).