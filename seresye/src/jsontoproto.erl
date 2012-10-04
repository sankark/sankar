-module(jsontoproto). 
-export([start/0,to_proto/1]). 
start() -> 
	Pid = spawn(fun() -> loop() end), 
	register(proto, Pid). 

loop() -> 
	
	receive {toProto, JsonString,From} -> 
				io:format("inside to proto"),
				{ok,HostName}=inet:gethostname(),
				ServerNode=list_to_atom("servernode@"++HostName),
				{asdas,ServerNode} ! {toProto,JsonString,self()} end,
	receive {result, Reply} -> 
				io:format("Res~p",[Reply]),
				From ! {proto, {result, Reply}}, 
				loop() end. 

to_proto(JsonString)->
	proto ! {toProto,JsonString,self()}, 
	receive {proto, {result,Reply}} -> 
				nodes:decode_request(Reply) end.
