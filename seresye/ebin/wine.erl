-module (wine).
-include ("wine.hrl").
-export ([is_class/1, is_a/2, 'wine'/1,'red-wine'/1, childof/1]).

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

