%% Author: Administrator
%% Created: Aug 9, 2012
%% Description: TODO: Add description to rules_service
-module(rules_service).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%

add_rule(RuleName,Pattern,Arity,Cond,Action)->
	Dest="C:/ErlangTools/seresye/src/rules/",
	Header=lists:flatten(io_lib:format("-module(~s).\n-export([~s/~w]).\n-rules([~s]).\n",
                                    [RuleName,
									 RuleName,Arity,
									 RuleName])),
	Lines=lists:flatten(io_lib:format("~s(Engine,~s) when ~s ->\n~s.",
                                    [RuleName,
                                     Pattern,
                                     Cond,Action])),
	Module=Dest++RuleName ,
	{ok, RulesFile} = file:open(Module++".erl",[write]),
	io:format(RulesFile,"~s~s",[Header,Lines]),
	file:close(RulesFile),
	rules_compiler:compile_rules(Module),
    seresye:add_rules(default,list_to_atom(filename:basename(Module))).
	

%%
%% Local Functions
%%

test()->
	add_rule("test","{test,test},{test2,X}",3,"X>10","seresye_engine:assert([{test3,test3}])").