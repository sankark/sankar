-module(build_response).
-include("../records/proto_template_pb.hrl").
-export([build_response/2]).
-rules([build_response]).
build_response(Engine,{build_response})  ->
Nodes=accumulate:accum(node,Engine,[{heap,gt,60}]),
Resp=#response{kb=#knowledgebase{nodes=Nodes},cs=#clientstate{action=seresye_engine:get_client_state(Engine)}},
seresye_engine:assert(Engine,Resp).
