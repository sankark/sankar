%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (node_usecase2).
-export ([heap_below_20/2,heap_below_20_2/3,heap_above_60/2,heap_above_60_2/3,tot_nodes/2,tot_nodes_2/3,tot_heap/2,tot_heap_2/3,heap_above60/3,heap_below20/3,avg_heap/3,init/2,start/0]).


-record (node, {mem_id,heap}).
-record (nodes, {nodes}).


-include_lib("eunit/include/eunit.hrl").


-rules([heap_below_20,heap_below_20_2,heap_above_60,heap_above_60_2,tot_heap,tot_heap_2,init,tot_nodes,tot_nodes_2]).


heap_above60(Engine,{heap_above,60,X},{tot_nodes,Y}) when (X > 0) and (X/Y*100 >= 75) ->
	seresye_engine:set_client_state(Engine,
                                    [scaleup | seresye_engine:get_client_state(Engine)]).

heap_below20(Engine,{heap_below,20,X},{tot_nodes,Y}) when (X > 0) and (X/Y*100 >= 75) ->
	seresye_engine:set_client_state(Engine,
                                    [scaledown | seresye_engine:get_client_state(Engine)]).

avg_heap(Engine,{tot_heap,X},{tot_nodes,Y}) when (X > 0) and (Y > 0) ->
		Engine2 = seresye_engine:assert (Engine,{avg_heap,X/Y}),
	 seresye_engine:set_client_state(Engine2,
                                    [seresye_engine:get_client_state(Engine2)]).
%tot_nodes(Engine,#nodes{nodes=Nodes}) ->
		%Engine1 = seresye_engine:assert (Engine,{tot_nodes,length(Nodes)}),
	 %seresye_engine:set_client_state(Engine1,
                     %              [seresye_engine:get_client_state(Engine1)]).

tot_nodes_2(Engine,{new_node}=F,{tot_nodes,X}=G)->
		Engine1=seresye_engine:retract(Engine,F),
		Engine2=seresye_engine:retract(Engine1,G),
		Engine3=seresye_engine:assert(Engine2,{tot_nodes,X+1}),
		seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).
tot_nodes(Engine,#node{})->
	io:format("tot_nodes~n"),
	Engine1=seresye_engine:assert(Engine,{new_node}),
	seresye_engine:set_client_state(Engine1,
                                    [seresye_engine:get_client_state(Engine1)]).

loop_a() ->
    receive
      stop -> ok;
      {msg, _Msg, 0} -> loop_a();
      {msg, Msg, N} ->  
		io:format("ping!~n"), 
        timer:sleep(500),
        b  !  {msg,  Msg,  N  -  1},
        loop_a()
     end.


tot_heap_2(Engine,{heap,Heap}=F,{tot_heap,X}=G)->
		Engine1=seresye_engine:retract(Engine,F),
		Engine2=seresye_engine:retract(Engine1,G),
		Engine3=seresye_engine:assert(Engine2,{tot_heap,X+Heap}),
		seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).
tot_heap(Engine,#node{heap=Heap})->
	io:format("tot_nodes~n"),
	Engine1=seresye_engine:assert(Engine,{heap,Heap}),
	seresye_engine:set_client_state(Engine1,
                                    [seresye_engine:get_client_state(Engine1)]).

heap_above_60_2(Engine,{heap_above_60}=F,{heap_above,60,X}=G)->
		Engine1=seresye_engine:retract(Engine,F),
		Engine2=seresye_engine:retract(Engine1,G),
		Engine3=seresye_engine:assert(Engine2,{heap_above,60,X+1}),
		seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).
heap_above_60(Engine,#node{heap=Heap}) when Heap > 60 ->
	Engine3 = seresye_engine:assert (Engine,{heap_above_60}),
	seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).
heap_below_20(Engine,#node{heap=Heap}) when Heap < 20 ->
	Engine3 = seresye_engine:assert (Engine,{heap_below_20}),
	seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).

heap_below_20_2(Engine,{heap_below_20}=F,{heap_below,20,X}=G)->
		Engine1=seresye_engine:retract(Engine,F),
		Engine2=seresye_engine:retract(Engine1,G),
		Engine3=seresye_engine:assert(Engine2,{heap_below,20,X+1}),
		seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).

init(Engine,#nodes { nodes = Nodes }) ->
	Engine5 = seresye_engine:assert (Engine,{tot_nodes,0}),
	Engine6 = seresye_engine:assert (Engine5,{tot_heap,0}),
	Engine3 = seresye_engine:assert (Engine6,{heap_above,60,0}),
	Engine4 = seresye_engine:assert (Engine3,{heap_below,20,0}),
	Engine2=lists:foldl(fun (Node,Engine1) -> seresye_engine:assert (Engine1,Node) end, Engine4 ,Nodes),

	seresye_engine:set_client_state(Engine2,
                                    [seresye_engine:get_client_state(Engine2)]).
	
start () ->
	Engine=seresye_engine:new([]),
    Engine0 = seresye_engine:add_rule (Engine,{?MODULE,init},3),
	 Engine1 = seresye_engine:add_rule (Engine0,{?MODULE,tot_nodes},2),
	 Engine2 = seresye_engine:add_rule (Engine1,{?MODULE,tot_nodes_2},2),
		    Engine3 = seresye_engine:add_rule (Engine2,{?MODULE,tot_heap_2},2),
	    Engine4 = seresye_engine:add_rule (Engine3,{?MODULE,tot_heap},2),
	Engine5 = seresye_engine:add_rule (Engine4,{?MODULE,heap_above_60},2),
	Engine6 = seresye_engine:add_rule (Engine5,{?MODULE,heap_above_60_2},2),
	Engine7 = seresye_engine:add_rule (Engine6,{?MODULE,heap_below_20},2),
	Engine8 = seresye_engine:add_rule (Engine7,{?MODULE,heap_below_20_2},2),
		Engine11 = seresye_engine:add_rule (Engine8,{?MODULE,heap_above60},1),
	Engine12 = seresye_engine:add_rule (Engine11,{?MODULE,heap_below20},1),
	

	Engine9 = seresye_engine:add_rule(Engine12,{?MODULE,avg_heap},0),
	
    Engine10 = seresye_engine:assert (Engine9, [#nodes { nodes = [#node{mem_id=1,heap=10},#node{mem_id=2,heap=10},#node{mem_id=3,heap=10},#node{mem_id=4,heap=100}]}]),

State = seresye_engine:get_client_state(Engine10),
	io:format("~n~p",[Engine10]),
io:format("~n~p",[lists:flatten(State)]).

