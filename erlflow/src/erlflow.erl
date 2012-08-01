%% Authou: josemanuelp
%% Created: 17/05/2008
%% Description: TODO: Add description to erlflow
-module(erlflow).

-export([start/0,init/0]).

%%
%% API Functions
%%

start() -> 
    register(netsuper, spawn(erlflow_net, netsuper,[[]])),
    erlflow_xpdl_parser:start(),
    {ok, Files} = file:list_dir("../public/pdefs"),
    load_nets(Files),
    mnesia:start(),
    inets:start(),
    application:start(ecouch),
    done.

init() ->
    db:init().

load_nets([Head|Tail]) ->
    io:format("~p~n", [Head]),
    Ext = lists:sublist(string:tokens(Head,"."),2,1),
    if Ext == ["xpdl"] ->
    	erlflow_xpdl_parser:process("../public/pdefs/" ++ Head);
   	true ->
      	[]
    end,
    load_nets(Tail);
load_nets([]) -> [].

%%
%% Local Functions
%%

