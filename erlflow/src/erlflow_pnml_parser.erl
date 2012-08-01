-module(erlflow_pnml_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([process/1]).


get_id_attr(Node) ->
    #xmlElement{attributes=Attribs} = Node,
    [ #xmlAttribute{name=id, value=ID}] = Attribs,
    list_to_atom(ID). 

get_id_attr_arc(Node) ->
    #xmlElement{attributes=Attribs} = Node,
    [ #xmlAttribute{name=target, value=Target},  #xmlAttribute{name=source, value=Source},  #xmlAttribute{name=id, value=ID}] = Attribs,
    {list_to_atom(ID), list_to_atom(Target), list_to_atom(Source)}. 

extract_name(R, L) when is_record(R, xmlElement) ->
    lists:foldl(fun extract_name/2, L, R#xmlElement.content);
extract_name(#xmlText{parents=[{text,_},{name,_},{_,_},{net,_},{pnml,_}], value=V}, _) ->
    V;
extract_name(_, L) ->
    L.

create_object([Node|Rest]) ->
    case Node#xmlElement.name of
        place ->
            ID = get_id_attr(Node),
            Name = extract_name(Node, []),
            register(ID, spawn(erlflow_net, place, [ID,Name,[],[],[]])),
            netsuper ! {add_place, ID},
            io:format("new place ~s:~p ~n", [ID, Name]),
            create_object(Rest);
        transition ->
            ID = get_id_attr(Node),
            Name = extract_name(Node, []),
            register(ID, spawn(erlflow_net, transition, [ID,Name,[],[]])),
            netsuper ! {add_transition, ID},
            io:format("new transition ~s:~p ~n", [ID, Name]),
            create_object(Rest);
        arc ->
            ID = get_id_attr_arc(Node),
            {_, Target, Source} = ID,
            Source ! {output, Target},
            Target ! {input, Source},
            io:format("new arc ~w~n", [ID]),
            create_object(Rest);
        _else ->
            create_object(Rest)
    end;
create_object([]) ->
    done.

process(PnmlFile) ->
    {Xml, _} = xmerl_scan:file(PnmlFile),
    create_object(xmerl_xpath:string("/pnml/net/child::*", Xml)),
    registered().
