-module(heap_gt_60).
-include("../records/node.hrl").
-include("../records/person.hrl").
-include("../records/sample.hrl").
-export([heap_gt_60/2]).
-rules([heap_gt_60]).
heap_gt_60(Engine0,{heap60,nodes,Node}) when length(Node) > 2 ->
seresye_engine:set_client_state(Engine0,[success|seresye_engine:get_client_state(Engine0)]).