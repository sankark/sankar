%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (node_usecase).
-export ([heap_above60/3,heap_below20/3,avg_heap/3,tot_nodes/2,tot_heap/2,node/3,init/2,start/0]).


-record (node, {mem_id,heap}).
-record (nodes, {nodes}).


-include_lib("eunit/include/eunit.hrl").

-rules([heap_above60,avg_heap,tot_nodes,tot_heap,node,init,heap_below20]).


heap_above60(Engine,{heap_above,60,X},{tot_nodes,Y}) when (X > 0) and (X/Y*100 >= 75) ->
	seresye_engine:set_client_state(Engine,
                                    [scaleup | seresye_engine:get_client_state(Engine)]).

heap_below20(Engine,{heap_below,20,X},{tot_nodes,Y}) when (X > 0) and (X/Y*100 >= 75) ->
	seresye_engine:set_client_state(Engine,
                                    [scaledown | seresye_engine:get_client_state(Engine)]).

avg_heap(Engine,#nodes{nodes=Nodes},{tot_heap,X}) when X > 0 ->
		Engine1 = seresye_engine:assert (Engine,{avg_heap,X/length(Nodes)}),
	 seresye_engine:set_client_state(Engine1,
                                    [seresye_engine:get_client_state(Engine1)]).
tot_nodes(Engine,#nodes{nodes=Nodes}) ->
		Engine1 = seresye_engine:assert (Engine,{tot_nodes,length(Nodes)}),
	 seresye_engine:set_client_state(Engine1,
                                    [seresye_engine:get_client_state(Engine1)]).

tot_heap(Engine,#nodes{nodes=Nodes})->
	Tot=lists:foldl(fun (#node{heap=Heap},Acc) -> Acc+Heap end, 0 ,Nodes),					
	Engine1 = seresye_engine:assert (Engine,{tot_heap,Tot}),
	 seresye_engine:set_client_state(Engine1,
                                    [seresye_engine:get_client_state(Engine1)]).
node(Engine,#node{heap=Heap}=F,{heap_above,60, X} = G) when Heap > 60 ->
	Engine1 = seresye_engine:retract(Engine,F),
	Engine2 = seresye_engine:retract(Engine1,G),
	Engine3 = seresye_engine:assert (Engine2,{heap_above,60,X+1}),
	seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]);

node(Engine,#node{heap=Heap}=F,{heap_below,20, X} = G) when Heap < 20 ->
	Engine1 = seresye_engine:retract(Engine,F),
	Engine2 = seresye_engine:retract(Engine1,G),
	Engine3 = seresye_engine:assert (Engine2,{heap_below,20,X+1}),
	seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).
init(Engine,#nodes { nodes = Nodes }) ->
	Engine2=lists:foldl(fun (Node,Engine1) -> seresye_engine:assert (Engine1,Node) end, Engine ,Nodes),
	Engine3 = seresye_engine:assert (Engine2,{heap_above,60,0}),
	Engine4 = seresye_engine:assert (Engine3,{heap_below,20,0}),
	seresye_engine:set_client_state(Engine4,
                                    [seresye_engine:get_client_state(Engine4)]).

start () ->
    Engine0 = seresye_engine:add_rules (seresye_engine:new([]), ?MODULE),
    Engine1 = seresye_engine:assert (Engine0, [#nodes { nodes = [#node{mem_id=1,heap=10},#node{mem_id=2,heap=10},#node{mem_id=3,heap=10},#node{mem_id=4,heap=100}]}]),
State = seresye_engine:get_client_state(Engine1),
	io:format("~nResult ~n~p",[lists:flatten(State)]).