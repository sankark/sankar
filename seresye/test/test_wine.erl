%% Author: Administrator
%% Created: Aug 1, 2012
%% Description: TODO: Add description to test_wine
-module(test_wine).

%%
%% Include files
%%
-include ("wine.hrl").
%%
%% Exported Functions
%%
-export([start/0,my_rule/2]).

%%
%% API Functions
%%



%%
%% Local Functions
%%
my_rule ( Engine , # wine {name=} = W) ->
	seresye_engine:set_client_state(Engine,
                                    [rulewine | seresye_engine:get_client_state(Engine)]);
my_rule ( Engine , W) ->
	%io:format("~p~n",[wine:wine (W)]),
	my_rule (Engine , wine:wine (W)).

start () ->
	 Engine0 = seresye_engine:add_rule (seresye_engine:new(test), {test_wine, my_rule}),
    Engine1 = seresye_engine:assert (Engine0, [#'white-wine'{name=[],color='black',flavor=[],grape=[],sugar=[]},
											   [{ciao, mondo}, {mondo, 20}],
                                               {hello, world},
                                               {ok, world}]),
	 State = seresye_engine:get_client_state(Engine1),
	io:format("Result ~n~p",[lists:flatten(State)]).
