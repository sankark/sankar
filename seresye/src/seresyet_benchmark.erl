%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_benchmark).
-export ([rule/2,start/1,test/0]).
-record (node, { node_id,heap,cpu}).
-define(LOG(Fmt,Msg),io:format(Fmt,Msg)).

rule (Engine, #node{node_id=Node,heap=Heap}=F) when Heap > 60 ->
	
    Engine.



start (Pool) ->
	Facts=[#node{node_id="node1",heap=70,cpu=10},#node{node_id="node2",heap=70,cpu=10},#node{node_id="node3",heap=70,cpu=10}
											  #node{node_id="node4",heap=70,cpu=10},#node{node_id="node5",heap=70,cpu=10},#node{node_id="node6",heap=70,cpu=10}
		  ,#node{node_id="node7",heap=70,cpu=10},#node{node_id="node8",heap=70,cpu=10},#node{node_id="node9",heap=70,cpu=10}
											  #node{node_id="node10",heap=70,cpu=10},#node{node_id="node11",heap=70,cpu=10},#node{node_id="node12",heap=70,cpu=10}],
	 lists:foldl(fun({Name,_},_A)->
	   worker_engine:assert (Name,Facts),
	
	   worker_engine:stop(Name) end,[],Pool).
 

build_worker_names(Prefix,0,Acc)->
	[list_to_atom(Prefix++integer_to_list(0))|Acc];
build_worker_names(Prefix,Counter,Acc) when Counter > 0->
	build_worker_names(Prefix,Counter-1,[list_to_atom("server"++integer_to_list(Counter))|Acc]).
	
create_worker_pool()->
WorkerNames=build_worker_names("server",100,[]),		
	lists:foldl(fun(H,A)-> [{H,worker_engine:start_link(H)}|A] end, [], WorkerNames).
			

test()->
	base_engine:start_link(),
	base_engine:add_rule({seresyet_benchmark,rule},0),
	
	
	
	Fun=fun()->
	Pool=create_worker_pool(),
	{Time,_Value}=timer:tc(seresyet_benchmark,start,[Pool]),
	io:format("~n~p",[(Time/1000)/100])
	end,
	loop(Fun,100),
	base_engine:stop().


loop(Fun,0)->
	[];
loop(Fun,Times)->
	loop(Fun,Times-1),
	Fun().