%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-28
%% @doc Open a dialog that asks confirmation to delete a predicate.

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

-module(action_default_delete_dialog).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_predicate_dialog, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the predicate.
%% @todo Check if the predicate is in use, if so show text that the predicate is in use and can't be deleted
%% @spec event(Event, Context1) -> Context2
event(#postback{message={delete_predicate_dialog, Id, OnSuccess}}, Context) ->
    case z_acl:is_allowed(insert, predicate, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess},
                {id, Id}
            ],
            z_render:dialog("Confirm delete", "_action_dialog_predicate_delete.tpl", Vars, Context);
        false ->
            z_render:growl_error("You are not allowed to delete predicates.", Context)
    end.
