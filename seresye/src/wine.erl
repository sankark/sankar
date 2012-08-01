-module (wine).
-include ("wine.hrl").
-export ([is_class/1, is_a/2, 'wine'/1,'red-wine'/1, childof/1, test/0, start/0,my_rule/2]).

is_class ('wine_grape') -> true;
is_class ('wine') -> true;
is_class ('red-wine') -> true;
is_class ('white-wine') -> true;
is_class ('Chianti') -> true;
is_class (_) -> false.

is_a ('red-wine','wine') -> true;
is_a ('white-wine','wine') -> true;
is_a ('Chianti','red-wine') -> true;
is_a ('Chianti','wine') -> true;
is_a (_,_) -> false.

childof ('wine_grape') -> [];
childof ('wine') -> ['red-wine','white-wine','Chianti'];
childof ('red-wine') -> ['Chianti'];
childof ('white-wine') -> [];
childof ('Chianti') -> [];
childof (_) -> exit (undef_class).

'wine' (X = #'red-wine'{}) ->
  #'wine'{
    'name' = X#'red-wine'.'name',
    'color' = X#'red-wine'.'color',
    'flavor' = X#'red-wine'.'flavor',
    'grape' = X#'red-wine'.'grape',
    'sugar' = X#'red-wine'.'sugar'};

'wine' (X = #'white-wine'{}) ->
  #'wine'{
    'name' = X#'white-wine'.'name',
    'color' = X#'white-wine'.'color',
    'flavor' = X#'white-wine'.'flavor',
    'grape' = X#'white-wine'.'grape',
    'sugar' = X#'white-wine'.'sugar'};

'wine' (X = #'Chianti'{}) ->
  #'wine'{
    'name' = X#'Chianti'.'name',
    'color' = X#'Chianti'.'color',
    'flavor' = X#'Chianti'.'flavor',
    'grape' = X#'Chianti'.'grape',
    'sugar' = X#'Chianti'.'sugar'}.

'red-wine' (X = #'Chianti'{}) ->
  #'red-wine'{
    'name' = X#'Chianti'.'name',
    'color' = X#'Chianti'.'color',
    'flavor' = X#'Chianti'.'flavor',
    'grape' = X#'Chianti'.'grape',
    'sugar' = X#'Chianti'.'sugar'}.


my_rule ( Engine , {X}) ->
	seresye_engine:set_client_state(Engine,
                                    [seresye_engine:get_client_state(Engine)]);
my_rule ( Engine , {X,Y}) ->
	seresye_engine:set_client_state(Engine,
                                    [seresye_engine:get_client_state(Engine)]);

my_rule ( Engine , # wine {} = W) ->
	seresye_engine:set_client_state(Engine,
                                    [rulewine | seresye_engine:get_client_state(Engine)]);
my_rule ( Engine , W) ->
	%io:format("~p~n",[wine:wine (W)]),
	my_rule (Engine , wine:wine (W)).
start () ->
	
io:format("mod name ~p",[?MODULE]),
   Engine0 = seresye_engine:add_rules (seresye_engine:new([]), seresyet_sample),
 Engine2 = seresye_engine:add_rule (Engine0, {?MODULE,my_rule}),
    Engine1 = seresye_engine:assert (Engine2, [#'white-wine'{name=[],color='black',flavor=[],grape=[],sugar=[]},
											   [{ciao, mondo}, {mondo, 20}],
                                               {hello, world},
                                               {ok, world}]),


State = seresye_engine:get_client_state(Engine1),
	io:format("Result ~n~p",[lists:flatten(State)]).

test()->
	wine:wine(#'red-wine'{name=[],color=[],flavor=[],grape=[],sugar=[]}).

