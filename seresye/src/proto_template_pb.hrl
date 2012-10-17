-ifndef(REQUEST_PB_H).
-define(REQUEST_PB_H, true).
-record(request, {
    req_name = "nodes",
    kb = erlang:error({required, kb})
}).
-endif.

-ifndef(RESPONSE_PB_H).
-define(RESPONSE_PB_H, true).
-record(response, {
    kb = erlang:error({required, kb}),
    cs = erlang:error({required, cs})
}).
-endif.

-ifndef(KNOWLEDGEBASE_PB_H).
-define(KNOWLEDGEBASE_PB_H, true).
-record(knowledgebase, {
    nodes = []
}).
-endif.

-ifndef(CLIENTSTATE_PB_H).
-define(CLIENTSTATE_PB_H, true).
-record(clientstate, {
    action = []
}).
-endif.

-ifndef(NODE_PB_H).
-define(NODE_PB_H, true).
-record(node, {
    node_id = erlang:error({required, node_id}),
    cpu,
    heap
}).
-endif.

