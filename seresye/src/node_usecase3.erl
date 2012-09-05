%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (node_usecase3).
-export ([start/0,test1/3,test2/2,test3/2,test/2,test4/2,tot_nodes/3,test/3,init/2]).


-record (node, {mem_id,heap}).
-record (nodes, {nodes}).


-include_lib("eunit/include/eunit.hrl").


-rules([test1,test3,test2,test4,tot_nodes,test,init]).



test(Engine,{testx}) ->
	io:format("~n inside {nothing}"),
	seresye_engine:set_client_state(Engine,
                                    [seresye_engine:get_client_state(Engine)]).
test1(Engine,{test},{test1}=F) ->
	io:format("~n inside {test}{test1}"),
E1=seresye_engine:retract(Engine, F),
	E2=seresye_engine:assert(E1, {testx}),
	seresye_engine:set_client_state(E2,
                                    [seresye_engine:get_client_state(E2)]).
test2(Engine,{test}) ->
	io:format("~n inside {test}"),
	seresye_engine:set_client_state(Engine,
                                    [seresye_engine:get_client_state(Engine)]).

test3(Engine,{test1}=F) ->
	io:format("~n inside {test1}"),
	E1=seresye_engine:retract(Engine, F),
	seresye_engine:set_client_state(E1,
                                    [seresye_engine:get_client_state(E1)]).
test4(Engine,{init})-> 
	io:format("~n inside init"),
	Engine3 = seresye_engine:assert (Engine,{tot_nodes,0}),
	seresye_engine:set_client_state(Engine3,
                                    [seresye_engine:get_client_state(Engine3)]).

test(Engine,{test}=Y,{tot_nodes,X}=N)->
	io:format("~n inside ok~p",[X]),
	Engine1=seresye_engine:retract(Engine,N),
	Engine2=seresye_engine:retract(Engine1,Y),
	Engine3=seresye_engine:assert(Engine2,{tot_nodes,X+1}),
		seresye_engine:set_client_state(Engine3,
                                    [x|seresye_engine:get_client_state(Engine3)]).


tot_nodes(Engine,#node{}=F,{tot_nodes,X}=G)->
   io:format("~n inside tot_nodes"),
   Engine1=seresye_engine:retract(Engine,G),
   Engine2=seresye_engine:assert(Engine1,{test}),
	seresye_engine:set_client_state(Engine2,
                                    [seresye_engine:get_client_state(Engine2)]).
	
init(Engine,{data,Data}=G) ->
	Engine1=seresye_engine:retract(Engine,G),
	Engine4 = seresye_engine:assert (Engine1,{tot_nodes,0}),
	Engine2=lists:foldl(fun (Node,Engine1) -> seresye_engine:assert (Engine1,Node) end, Engine4 ,Data),

	seresye_engine:set_client_state(Engine2,
                                    [seresye_engine:get_client_state(Engine2)]).

start() ->
    Engine0 = seresye_engine:add_rules (seresye_engine:new([]), node_usecase3),
    Engine1 = seresye_engine:assert (Engine0,{data,[#node{mem_id=1,heap=10},#node{mem_id=2,heap=10},#node{mem_id=3,heap=10},#node{mem_id=4,heap=100}]}),
	io:format("~p",[Engine1]).


