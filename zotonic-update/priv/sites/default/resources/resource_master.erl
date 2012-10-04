%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell, Arjan Scherpenisse
%% @doc Admin webmachine_resource.

%% Copyright 2009-2010 Marc Worrell, Arjan Scherpenisse
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

-module(resource_master).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    is_authorized/2,
    event/2,
    filter_props/1,
	ensure_id/1
]).

-include_lib("../../../../include/resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    {Context2, Id} = ensure_id(?WM_REQ(ReqData, Context)),
    z_acl:wm_is_authorized([{use, mod_admin}, {view, Id}], Context2).


resource_exists(ReqData, Context) ->
    {Context2, Id} = ensure_id(?WM_REQ(ReqData, Context)),
    case Id of
        undefined -> ?WM_REPLY(false, Context2);
        _N -> ?WM_REPLY(m_rsc:exists(Id, Context2), Context2)
    end.
        

html(Context) ->
    Id = z_context:get(id, Context),
    Template = z_context:get(template, Context),
	Result=m_master:getAll(master,Context),
	io:format("Result ~p",[Result]),
    Html = z_template:render(Template, [{result,Result}], Context),
	z_context:output(Html, Context).


%% @doc Fetch the (numerical) page id from the request
ensure_id(Context) ->
    Context2 = z_context:ensure_all(Context),
    case z_context:get(id, Context2) of
        N when is_integer(N) ->
            {Context2, N};
        undefined ->
            try
                {ok, IdN} = m_rsc:name_to_id(z_context:get_q("id", Context2), Context2),
                {z_context:set(id, IdN, Context2), IdN}
            catch
                _:_ -> {Context2, undefined}
            end
    end.


%% @doc Handle the submit of the resource edit form
event(#submit{message=rscform}, Context) ->
    Post = z_context:get_q_all_noz(Context),
    Props = filter_props(Post),
    Id = z_convert:to_integer(proplists:get_value("id", Props)),
    Props1 = proplists:delete("id", Props),
    CatBefore = m_rsc:p(Id, category_id, Context),
    case m_rsc:update(Id, Props1, Context) of
        {ok, _} -> 
            case proplists:is_defined("save_view", Post) of
                true ->
                    PageUrl = m_rsc:p(Id, page_url, Context),
                    z_render:wire({redirect, [{location, PageUrl}]}, Context);
                false ->
                    case m_rsc:p(Id, category_id, Context) of
                        CatBefore ->
                            Context1 = z_render:set_value("field-name", m_rsc:p(Id, name, Context), Context),
                            Context2 = z_render:set_value("field-uri",  m_rsc:p(Id, uri, Context1), Context1),
                            Context3 = z_render:set_value("field-page-path",  m_rsc:p(Id, page_path, Context1), Context2),
                            Context4 = z_render:set_value("slug",  m_rsc:p(Id, slug, Context3), Context3),
                            Context4b= z_render:set_value("visible_for", integer_to_list(m_rsc:p(Id, visible_for, Context4)), Context4),
                            Context5 = case z_convert:to_bool(m_rsc:p(Id, is_protected, Context4b)) of
                                true ->  z_render:wire("delete-button", {disable, []}, Context4b);
                                false -> z_render:wire("delete-button", {enable, []}, Context4b)
                            end,
                            Title = ?__(m_rsc:p(Id, title, Context5), Context5),
                            Context6 = z_render:growl(["Saved “", Title, "”."], Context5),
                            case proplists:is_defined("save_duplicate", Post) of
                                true ->
                                    z_render:wire({dialog_duplicate_rsc, [{id, Id}]}, Context6);
                                false ->
                                    Context6
                            end;
                        _CatOther ->
                            z_render:wire({reload, []}, Context)
                    end
            end;
        {error, duplicate_uri} ->
            z_render:growl_error("Error, duplicate uri. Please change the uri.", Context);
        {error, duplicate_name} ->
            z_render:growl_error("Error, duplicate name. Please change the name.", Context);
        {error, eacces} ->
            z_render:growl_error("You don't have permission to edit this page.", Context);
        {error, invalid_query} ->
            z_render:growl_error("Your search query is invalid. Please correct it before saving.", Context);
        {error, _Reason} ->
            z_render:growl_error("Something went wrong. Sorry.", Context)
    end;

event(#postback{message={test_data, Opts}}, Context) ->
   Data=z_context:get_q("test_data", Context),
   io:format("dsfsdfdsdfafda~p",[Context]),
   Context;

event(#submit{message=add_master}, Context) ->
	    Post = z_context:get_q_all_noz(Context),
    Props = filter_props(Post),
		Props2=proplists:delete(is_authoritative, Props) ,
		io:format("inside postback ~p",[Context]),
		MasterFun=proplists:get_value("master_code", Props2),
		rules_service:generate_master(MasterFun),
		m_master:insert(Props2,Context),
		z_render:growl(["Model Added"], Context),
		 z_render:wire({reload, []}, Context);
     
event(#postback{message={typeselect, Cats, Template, Actions, ActionsWithId, OtherArgs}, target=TargetId}, Context) ->
    Text = z_context:get_q("triggervalue", Context),
    Props = [{cat,Cats}, {text, Text}],
    Result = z_search:search({autocomplete, Props}, {1,20}, Context),
    M = #m{
        model=m_search,
        value=#m_search_result{result=Result, total=20, search_name=autocomplete, search_props=Props}
    },
    Vars = [
        {result, M},
        {action, Actions},
        {action_with_id, ActionsWithId}
        | OtherArgs
    ],
    Html = z_template:render(Template, Vars, Context),
    z_render:update(TargetId, Html, Context);

event(#sort{items=Sorted, drop={dragdrop, {object_sorter, Props}, _, _}}, Context) ->
    RscId     = proplists:get_value(id, Props),
    Predicate = proplists:get_value(predicate, Props),
    EdgeIds   = [ EdgeId || {dragdrop, EdgeId, _, _ElementId} <- Sorted ],
    m_edge:update_sequence_edge_ids(RscId, Predicate, EdgeIds, Context),
    Context;
%%{% wire id=#query type="change" postback={query_preview rsc_id=id div_id=#querypreview} delegate="resource_admin_edit" %}


%<div class="query-results" id="{{ #querypreview }}">
%		{% include "_admin_query_preview.tpl" result=m.search[{query query_id=id pagelen=20}] %}
%	</div>

%% Previewing the results of a query in the admin edit
event(#postback{message={query_preview, Opts}}, Context) ->
    DivId = proplists:get_value(div_id, Opts),
    try
        Q = search_query:parse_query_text(z_context:get_q("triggervalue", Context)),
        S = z_search:search({'query', Q}, Context),
        {Html, Context1} = z_template:render_to_iolist("_admin_query_preview.tpl", [{result,S}], Context),
        z_render:update(DivId, Html, Context1)
    catch
        _: {error, {Kind, Arg}} ->
            z_render:growl_error(["There is an error in your query: ", Kind, " - ", Arg], Context)
    end.


%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
	
    Remove = [
        "triggervalue",
        "postback",
        "z_trigger_id",
        "z_pageid",
        "z_submitter",
        "trigger_value",
        "save_view",
        "save_duplicate",
        "save_stay",
		"is_authoritative",
		"test_result"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].
