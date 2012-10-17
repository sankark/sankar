%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-05-12
%% @doc Open a dialog with some fields to make a new predicate.

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

-module(action_default_search).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").
%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions = proplists:get_all_values(action, Args),
    ActionsWithId = proplists:get_all_values(action_with_id, Args),
    Cats = proplists:get_all_values(cat, Args),
    Template = proplists:get_value(template, Args, "search_record.tpl"),
    OtherArgs = proplists:delete(action, 
                    proplists:delete(action_with_id, 
                        proplists:delete(cat, 
                            proplists:delete(template, Args)))),
    Postback = {search, Cats, Template, Actions, ActionsWithId, OtherArgs},
    {_PostbackMsgJS, PickledPostback} = z_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
    JS = [
        <<"z_typeselect(\"">>, TriggerId, $",$,,$", PickledPostback, <<"\");">>
    ],
    {JS, Context}.


%% @doc Show possible completions of the search text using a template.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={search, Cats, Template, Actions, ActionsWithId, OtherArgs}, target=TargetId}, Context) ->
    Text = z_context:get_q("triggervalue", Context),
    Props = [{cat,Cats}, {text, Text}],
	%io:format("##############Text~s",[Text]),
	Exp_Records=model_service:get_exported_records(),
	Length=length(Text),
	S_Text= string:sub_string(Text, 1,Length),
	%io:format("Exp_Records~p",[Exp_Records]),
	Matched=lists:foldl(fun(H,A)-> 
						M=case S_Text =:= string:sub_string(atom_to_list(H),1,Length) of
							true -> H;
							false -> []
						end,
						[M|A]
				end,
											   [], Exp_Records),
	%io:format("Matched ~p",[Matched]),
	Result2=lists:foldl(fun(H,A)-> 
						R=case model_service:get_record(H) of
							"No Record Found" -> [];
							Record -> Record
						end,
					[R|A]	
				end,
											   [], Matched),
	
	%[Result2]=model_service:get_record(list_to_atom(string:to_lower(Text))),
	%io:format("record~p",[Result2]),
    Vars = [
        {result, Result2}
    ],
    Html = z_template:render(Template, Vars, Context),
    z_render:update(TargetId, Html, Context).
