%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-07
%%
%% @doc Initialize the database with start data.

%% Copyright 2009 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(install_rules).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/0
]).



%% @doc Insert boot data into the database.
%% @spec install(Host::atom(), Connection) -> ok

install()->
	ok = install_models(),
	ok = install_rules().


install_models()->
	Result=m_models:getAll(models,z_context:new(default)),
	R2=convert_bin_to_list(Result,[]),
	[proto_service:generate_model(Y, X)||[{model_name,X},{model_def,Y}]<-R2],
	ok.
install_rules()->
	Result=m_rules:getAll(rules,z_context:new(default)),
	[rules_service:add_rule(RuleName,Pattern,Condition,Action,State,Salience)||[{rule_name,RuleName},
{pattern,Pattern},{condition,Condition},{action,Action},{client_state,State},{salience,Salience}]<-convert_bin_to_list(Result,[])],
	MasterCode=m_master:getAll(master,z_context:new(default)),
	io:format("~p",[MasterCode]),
	[rules_service:generate_master(binary_to_list(Code))||[{master_code,Code}]<-MasterCode],
ok.


convert_bin_to_list([],Acc)->
io:format("~p",[lists:reverse(Acc)]),
Acc;
convert_bin_to_list([H|T],Acc)->
Result=lists:foldl(fun({Key,Value},Acc)->[{Key,binary_to_list(Value)}|Acc] end,  [], H),
convert_bin_to_list(T,[Result|Acc]).


