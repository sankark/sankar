%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_sample).
-export ([rule/4, rule1/3, rule2/4, rule3/3, start/0]).

-record (sample_record, { a = nil, b}).

-include_lib("eunit/include/eunit.hrl").

-rules([rule, rule1, rule2, rule3]).

rule (Engine, {hello, world}, {ciao, X}, {X, 10}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule | seresye_engine:get_client_state(Engine)]).

rule1 (Engine0, {hello, world}, {ciao, _X} = F) ->
    Engine1 = seresye_engine:assert (Engine0, {test}),
    Engine2 = seresye_engine:retract (Engine1, F),
    seresye_engine:set_client_state(Engine2,
                                    [rule1a | seresye_engine:get_client_state(Engine2)]);
rule1 (Engine, {hello, world}, {test}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule1b | seresye_engine:get_client_state(Engine)]).


rule2 (Engine, {hello, world}, #sample_record { a = Z }, {mondo, Z}) ->
	io:format("~p", [rule2]),
    seresye_engine:set_client_state(Engine,
                                    [rule2 | seresye_engine:get_client_state(Engine)]).

rule3 (Engine, {hello, [_H|T]}, {test, T}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule3 | seresye_engine:get_client_state(Engine)]).

start () ->
io:format("mod name ~p",[?MODULE]),
    Engine0 = seresye_engine:add_rules (seresye_engine:new([]), ?MODULE),
    Engine1 = seresye_engine:assert (Engine0, [[{ciao, mondo}, {mondo, 20}],
                                               {hello, world},
                                               {ok, world},{ssda},
                                               #sample_record { a = 10, b = 50}]),
State = seresye_engine:get_client_state(Engine1),
	io:format("Result ~n~p",[State]).