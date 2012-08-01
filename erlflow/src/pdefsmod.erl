-module(pdefsmod).
-author('josemanuelp@gmail.com').

-include("../include/yaws_api.hrl").
-compile(export_all).

out(A) ->
    {content, "application/x-xpdl",  showfile(A#arg.appmoddata)}.

showfile(FilePath) ->
    {ok, Cwd} = file:get_cwd(),
    FName = Cwd ++ "/public/pdefs/" ++ FilePath,
    io:format("pdefsmod:showfile -> reading file ~p~n", [FName]),
    {ok, _Data} = file:read_file(FName),
    io:format("pdefsmod:showfile -> data ~p~n", [_Data]),
    _Data.
