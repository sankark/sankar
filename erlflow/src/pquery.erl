-module(pquery).
-export([start/0,server/1]).

server(M) ->
	receive
		{ask, From} ->
			From ! {"respuesta"};
		_Other ->
			server(M)
	end.

start () ->
	Pid = spawn(pquery, server, [[]]),
	Pid ! {ask, self()},
	receive {Text} -> io:format("Response=~s~n", [Text]) end.
