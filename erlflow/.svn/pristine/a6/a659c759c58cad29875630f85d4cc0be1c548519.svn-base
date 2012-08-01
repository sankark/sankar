-module(test).
-export([start/0]).

start() ->
    netsuper ! {get_status, self()},
    receive Networks -> Net = Networks end,
    %lists:map(fun(X) -> if is_atom(X) -> atom_to_list(X); true -> X end end, Net).
    JsonVar = prepare_json([Net]).
    %JVar =  rfc4627:encode([sksksi,"sksks"]),
    %JVar = ktuo_json:encode([{string, "wwwdddd"},{string, "ksksks"}]),
    %JVar = json:encode(JsonVar),
    %$io:format("Json = ~p~n", [JsonVar]).

prepare_json({Key, Value}) ->
    io:format("thread was here ~n"),
    {Key, {string, Value}};
prepare_json([Head | Tail]) ->
    lists:merge([prepare_json(Head)],prepare_json(Tail));
prepare_json([]) -> [] .
	%prepare_json2(Head),
	%prepare_json([Tail])
