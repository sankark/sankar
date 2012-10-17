-module(prototojson). 
-export([start/0,to_json/1]). 
start() -> 
	Pid = spawn(fun() -> loop() end), 
	register(json, Pid). 

loop() -> 
	
	
	receive {toJson, ProtoBinary,From} -> 
				%io:format("inside to proto"),
				{ok,HostName}=inet:gethostname(),
				ServerNode=list_to_atom("servernode@"++HostName),
				{asdas,ServerNode} ! {toJson,ProtoBinary,self()} end,
	receive {result, Reply} -> 
				%io:format("Res~p",[Reply]),
				From ! {json, {result, Reply}}, 
				loop() end. 

to_json(Proto)->
	json ! {toJson,Proto,self()}, 
	receive {json, {result,Reply}} -> 
				{result,Reply} end.