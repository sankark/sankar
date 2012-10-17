%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_benchmark2).
-export ([rule/2,start/1,test/0]).
-record (node, { node_id,heap,cpu}).


rule (Engine, #node{heap=Heap,node_id=Node}=F) when Heap > 60 ->
	%io:format("~s~n",[Node]),
	Engine.


build_objects(0,Acc)->
	[#node{node_id="node"++integer_to_list(0),heap=70,cpu=10}|Acc];
build_objects(Counter,Acc) when Counter > 0->
	build_objects(Counter-1,[#node{node_id="node"++integer_to_list(Counter),heap=70,cpu=10}|Acc]).

start (0)->
	ok;
start (Counter) when Counter > 0 ->
	
		Name=server,
	worker_engine:start_link(Name),
	Facts=[#node{node_id="node"++integer_to_list(Id),heap=70,cpu=10}||Id<-lists:seq(1,100)],
	   worker_engine:assert (Name,Facts),
		worker_engine:stop(Name),
		start(Counter-1).
		



test()->
	base_engine:start_link(),
	base_engine:add_rule({seresyet_benchmark2,rule},0),
	Fun=fun(Acc)->
	{Time,_Value}=timer:tc(seresyet_benchmark2,start,[100]),
	[Time/100000|Acc]
	end,
	
	Lists=loop(Fun,100,[]),
	[Min]=lists:min(Lists),
	[Max]=lists:max(Lists),
	io:format("min=~p max=~p ~n, Avg = ~p",[Min,Max,lists:sum(lists:flatten(Lists))/100]),
	base_engine:stop().


loop(Fun,0,Acc)->
	Acc;
loop(Fun,Times,Acc)->
	loop(Fun,Times-1,[Fun([])|Acc]).
	
	   