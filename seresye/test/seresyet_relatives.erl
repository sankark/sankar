%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011, Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_relatives).
-behavior(gen_server).
-export([father/3, grandfather/3, grandmother/3,
         mother/3, brother/4, sister/4]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-include_lib("eunit/include/eunit.hrl").

-rules([mother, father, brother, sister, grandfather,
        grandmother]).


start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother (Engine, {female, X}, {parent, X, Y}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {mother, X, Y}),
                                    [{mother, X, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father (Engine, {male, X}, {parent, X, Y}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {father, X, Y}),
                                    [{father, X, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (Y and Z have the same parent X) and (Z is female)
%%    then (Z is Y's sister)
%%
sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {sister, Z, Y}),
                                    [{sister, Z, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (Y and Z have the same parent X) and (Z is male)
%%    then (Z is Y's brother)
%%
brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {brother, Z, Y}),
                                    [{brother, Z, Y} | seresye_engine:get_client_state(Engine)]).


%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {grandfather, X, Z}),
                                    [{grandfather, X, Z}
                                     | seresye_engine:get_client_state(Engine)]).

%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
    seresye_engine:set_client_state(seresye_engine:assert(Engine,
                                                          {grandmother, X, Z}),
                                    [{grandmother, X, Z}
                                     | seresye_engine:get_client_state(Engine)]).

init([]) -> 
    {ok, []}.

handle_call(_Call, _From, State) -> 
    {noreply, State}.
handle_cast(_Cast, State) -> 
    {noreply, State}.
handle_info(_Info, State) -> 
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

rules_test() ->
    Engine0 = seresye_engine:new([]),
    Engine2 =  seresye_engine:add_rules(Engine0, ?MODULE),

    Engine3 = seresye_engine:assert (Engine2,
                                     [[{male, kanagu}],
                                      {male, sankar},
                                      {female, valar},
                                      {female, rupa},
                                      {female, deepa},
                                      {parent, kanagu, sankar},
                                      {parent, kanagu, rupa},
                                      {parent, kanagu, deepa},
									   {parent, valar, sankar},
                                      {parent, valar, rupa},
                                      {parent, valar, deepa}]),
                                     

    InternalState = seresye_engine:get_client_state(Engine3),
	io:format("~p",[InternalState]),

    ?assertMatch(true,
                 lists:member({mother,valar,sankar}, InternalState)),

    ?assertMatch(true,
                 lists:member({sister,rupa,sankar},
                              InternalState)),

      ?assertMatch(true,
                 lists:member({sister,deepa,sankar},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({brother,sankar,rupa},
                              InternalState)),

    ?assertMatch(true,
                 lists:member({father,kanagu,sankar},
                              InternalState)),
    ?assertMatch(true,
                 lists:member({father,kanagu,rupa},
                              InternalState)),
   


    
    %?assertMatch(29, erlang:length(InternalState)),
	io:format("~p",[InternalState]).



