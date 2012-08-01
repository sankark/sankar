-module(erlflow_xpdl_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([start/0,process/1,net_sender_xpdl/2]).

start() ->
    register(net_sender_xpdl, spawn(erlflow_xpdl_parser, net_sender_xpdl, [[],[]])).

process(FilePath) -> 
    {R,_} = xmerl_scan:file(FilePath),
	io:format("erlflow_xpdl_parser:process -> reading XPDL file~n"),
    extract(R, []),
    PathParts = string:tokens(FilePath,"/"),

    FileName = lists:flatten(lists:sublist(PathParts,length(PathParts),1)),
	io:format("~p",[FileName]),
    net_sender_xpdl ! {send_msg, {set_info, {deffile, FileName}}}.	

net_sender_xpdl(Id, Name) ->
	receive
        {setup_net, _Id, _Name} ->
            io:format("erlflow_xpdl_parser:net_sender_xpdl -> setting up net ~s~n", [_Id]),
            netsuper ! {add_net, _Id, _Name},
            net_sender_xpdl(_Id, _Name);
        {send_msg, Msg} ->
            io:format("erlflow_xpdl_parser:net_sender_xpdl -> sending a message to ~p network~n~n", [Id]),
            %io:format("sending message ~w to ~s network~n~n", [Msg,[Id]]),
            Id ! Msg,
            net_sender_xpdl(Id, Name);
        _else ->
            io:format("erlflow_xpdl_parser:net_sender_xpdl -> net_sender_xpdl Received:~p~n", [_else]),
            net_sender_xpdl(Id, Name)
 	end.
            

extract(R, L) when is_record(R, xmlElement) ->
    %io:format("extract(~p).~n", [R]),
    case R#xmlElement.name of
        'xpdl:Package' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:Package~n"),
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:WorkflowProcesses' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:WorkflowProcesses~n"),
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:WorkflowProcess' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:WorkflowProcess~n"),
            FFunc = fun(X) -> X#xmlAttribute.name == 'Id' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            I = hd(lists:filter(FFunc, R#xmlElement.attributes)),
            FFunc2 = fun(X) -> X#xmlAttribute.name == 'Name' end,
            V = hd(lists:filter(FFunc2, R#xmlElement.attributes)),
            Id = I#xmlAttribute.value,
            Name = V#xmlAttribute.value,
 			net_sender_xpdl ! {setup_net, list_to_atom(Id), Name},
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:DataField' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:DataField~n"),
            FFunc = fun(X) -> X#xmlAttribute.name == 'Id' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            I = hd(lists:filter(FFunc, R#xmlElement.attributes)),
            FFunc2 = fun(X) -> X#xmlAttribute.name == 'Name' end,
            V = hd(lists:filter(FFunc2, R#xmlElement.attributes)),
            Id = I#xmlAttribute.value,
            Name = V#xmlAttribute.value,
            Length = lists:foldl(fun extract/2, [], R#xmlElement.content),
            Msg = {add_field, {Id, Name, Length}},
            net_sender_xpdl ! {send_msg, Msg},
            lists:foldl(fun extract/2, L, R#xmlElement.content);
           
        'xpdl:Activities' ->
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:Participants' ->
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:Participant' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:Participant~n"),
            FFuncId = fun(X) -> X#xmlAttribute.name == 'Id' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            I = hd(lists:filter(FFuncId, R#xmlElement.attributes)),
            FFuncName = fun(X) -> X#xmlAttribute.name == 'Name' end,
            _V = lists:filter(FFuncName, R#xmlElement.attributes),
            _Vlength = lists:flatlength(_V),
            NetPid = list_to_atom(I#xmlAttribute.value),
            if 
                _Vlength > 0 ->
            		V = hd(_V),
                    Msg = {add_participant, NetPid, V#xmlAttribute.value};
				_Vlength == 0 ->
                    Msg = {add_participant, NetPid, []}
			end,
            io:format("***net_sender_xpdl:~p~n", [{send_msg, Msg}]),
            net_sender_xpdl ! {send_msg, Msg},
            io:format("***net_sender_xpdl: done.~n"),
            io:format("***lists:foldl call.~n"),
            ItemData = lists:foldl(fun extract/2, [], R#xmlElement.content),
            io:format("erlflow_xpdl_parser:extract -> reading element: xpdl:Participant -> ~p~n", [ItemData]),
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:Activity' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:Activity~n"),
            FFuncId = fun(X) -> X#xmlAttribute.name == 'Id' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            I = hd(lists:filter(FFuncId, R#xmlElement.attributes)),
            FFuncName = fun(X) -> X#xmlAttribute.name == 'Name' end,
            _V = lists:filter(FFuncName, R#xmlElement.attributes),
            _Vlength = lists:flatlength(_V),
            NetPid = list_to_atom(I#xmlAttribute.value),
            if 
                _Vlength > 0 ->
            		V = hd(_V),
                    Msg = {add_place, NetPid, V#xmlAttribute.value};
				_Vlength == 0 ->
                    Msg = {add_place, NetPid, []}
			end,
            io:format("***net_sender_xpdl:~p~n", [{send_msg, Msg}]),
            net_sender_xpdl ! {send_msg, Msg},
            io:format("***net_sender_xpdl: done.~n"),
            io:format("***lists:foldl call.~n"),
            [Performer] = lists:foldl(fun extract/2, [], R#xmlElement.content),
            io:format("erlflow_xpdl_parser:extract -> reading element: xpdl:Activity -> ~p~n", [Performer]),
            NetPid ! {set_performer, Performer},
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:ExtendedAttribute' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:ExtendedAttribute~n"),
            FFuncName = fun(X) -> X#xmlAttribute.name == 'Name' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            N = hd(lists:filter(FFuncName, R#xmlElement.attributes)),
            FFuncValue = fun(X) -> X#xmlAttribute.name == 'Value' end,
            V = hd(lists:filter(FFuncValue, R#xmlElement.attributes)),
            Msg = {get_field, self(), V#xmlAttribute.value},
			net_sender_xpdl ! {send_msg, Msg},
			receive Response -> Response end, 
            Response,
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        'xpdl:Transition' ->
            io:format("erlflow_xpdl_parser:extract -> reading xpdl:Transition~n"),
            FFuncId = fun(X) -> X#xmlAttribute.name == 'Id' end,
            io:format("~p~n",[R#xmlElement.attributes]),
            I = hd(lists:filter(FFuncId, R#xmlElement.attributes)),
            FFuncName = fun(X) -> X#xmlAttribute.name == 'Name' end,
            V = lists:filter(FFuncName, R#xmlElement.attributes),
            Vlength = lists:flatlength(V),
            NetId = list_to_atom(I#xmlAttribute.value),
            if 
                Vlength > 0 ->
            		_V = hd(V),
                    Msg = {add_transition, NetId, _V#xmlAttribute.value};
				Vlength == 0 ->
                    Msg = {add_transition, NetId, []}
			end,
            io:format("***net_sender_xpdl:~p~n", [{send_msg, Msg}]),
            net_sender_xpdl ! {send_msg, Msg},
            io:format("***net_sender_xpdl: done.~n"),
            io:format("***lists:foldl call.~n"),
            
            FFuncFrom = fun(X) -> X#xmlAttribute.name == 'From' end,
            F = hd(lists:filter(FFuncFrom, R#xmlElement.attributes)),
            From = list_to_atom(F#xmlAttribute.value),
            
            FFuncTo = fun(X) -> X#xmlAttribute.name == 'To' end,
            T = hd(lists:filter(FFuncTo, R#xmlElement.attributes)),
            To = list_to_atom(T#xmlAttribute.value),
            
            From ! {output, NetId},
            NetId ! {input, From},
            NetId ! {output, To},
            To ! {input, NetId},
            
            lists:foldl(fun extract/2, L, R#xmlElement.content);
        'xpdl:ProcessHeader' ->
            ItemData = lists:foldl(fun extract/2, [], R#xmlElement.content),
            io:format("erlflow_xpdl_parser:extract -> reading element: xpdl:ProcessHeader -> ~p~n", [ItemData]),
            net_sender_xpdl ! {send_msg, {set_info, lists:flatten([ItemData])}},
			lists:foldl(fun extract/2, L, R#xmlElement.content);
        
        %item ->
        %    ItemData = lists:foldl(fun extract/2, [], R#xmlElement.content),
        %    [ ItemData | L ];
        
        _ -> 
            io:format("erlflow_xpdl_parser:extract -> reading element: ~p~n", [R#xmlElement.name]),
            lists:foldl(fun extract/2, L, R#xmlElement.content)
    end;

extract(#xmlText{parents=[{'xpdl:Length',_},{'xpdl:DataField',_},_,_,_,_], value=V}, L) ->
    [{length, V}|L];

extract(#xmlText{parents=[{'xpdl:Created',_},{'xpdl:ProcessHeader',_},_,_,_], value=V}, L) ->
    [{created, V}|L];

extract(#xmlText{parents=[{'xpdl:Description',_},{'xpdl:ProcessHeader',_},_,_,_], value=V}, L) ->
    [{description, V}|L]; 

extract(#xmlText{parents=[{'xpdl:Performer',_},{'xpdl:Activity',_},_,_,_,_], value=V}, L) ->
    [{performer, V}|L]; 

extract(#xmlText{parents=[{title,_},{channel,2},_], value=V}, L) ->
    [{channel, V}|L]; 

extract(#xmlText{parents=[{title,_},{item,_},_,_], value=V}, L) ->
    [{title, V}|L]; 

extract(#xmlText{parents=[{link,_},{item,_},_,_], value=V}, L) ->
    [{link, V}|L]; 

extract(#xmlText{parents=[{pubDate,_},{item,_},_,_], value=V}, L) ->
    [{pubDate, V}|L]; 

extract(#xmlText{parents=[{'dc:date',_},{item,_},_,_], value=V}, L) ->
    [{pubDate, V}|L];

extract(#xmlText{}, L) -> L.  

