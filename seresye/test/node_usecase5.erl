%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (node_usecase5).
-mod_prio(1).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export ([start/1,tot_nodes/3]).
-include("../src/records/node.hrl").
-rules([tot_nodes]).


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
tot_nodes(Engine,#node{}=F,{tot_nodes,X}=G)->
io:format("test"),
	Engine1=seresye_engine:retract(Engine,F),
	Engine3=seresye_engine:retract(Engine1,G),
   Engine2=seresye_engine:assert(Engine3,{tot_nodes,X+1}),
	seresye_engine:set_client_state(Engine2,
                                    [seresye_engine:get_client_state(Engine2)]).
	
init_data()->
	[{tot_nodes,0}].

start(Data) ->
	%Data=[#node{mem_id=1,heap=10},#node{mem_id=2,heap=10},#node{mem_id=3,heap=10},#node{mem_id=4,heap=100}],
   gen_server:call(?MODULE, {data,Data}).

get_engine() ->
	 seresye_engine:add_rules (seresye_engine:new([]), node_usecase5).
init([])->
  	{ok, []}.

	
handle_call({data,Data}, _From, State) -> 
	io:format("Hi~p",[get_engine()]),
	Engine2 = seresye_engine:assert (seresye_engine:add_rules (seresye_engine:new([]), node_usecase5),lists:flatten([Data|init_data()])),
	KB=seresye_engine:get_kb(Engine2),
	io:format("Hi~p",[KB]),
    {reply, Engine2,Engine}.
handle_cast(_Cast, State) -> 
    {noreply, State}.
handle_info(_Info, State) -> 
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.


