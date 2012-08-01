-module(erlflow_net).
-include_lib("xmerl/include/xmerl.hrl").

-export([network/5,netsuper/1,place/7,transition/4,performer/1]).

performer(Activities) ->
    receive
		{add_activity, _Activity} ->
			NewActivities = lists:append(Activities, [{activity, _Activity}]),
			performer(NewActivities);
		{get_activity, From} ->
            From ! {activities,  Activities},
			performer(Activities)
    end.

place(ID, Name, Inputs, Outputs, Tokens, Info, FormFields) ->
    receive
        {add_token, Token} ->
            io:format("erlflow_net:place -> token received: ~s ~n", [Token]),
            NewTokens = lists:append(Tokens, [Token]),
            place(ID, Name, Inputs, Outputs, NewTokens, Info, FormFields);
        {consume_token, TransitionPid, TokenId} ->
            io:format("erlflow_net:place -> ~p is consuming token: ~p~n", [ID, TokenId]),
            Bool = lists:any(fun(X) -> X == TokenId end, Tokens),
            if 
                Bool ->
                    TransitionPid ! {token_consumed, TokenId},       
                    NewTokens = lists:delete(TokenId, Tokens),
                    place(ID, Name, Inputs, Outputs, NewTokens, Info, FormFields);
                Bool == false ->
                    io:format("erlflow_net:place -> ~p doesn't have token ~p ~n", [ID, TokenId]),
                    place(ID, Name, Inputs, Outputs, Tokens, Info, FormFields)
            end;
        {input, Source} ->
            io:format("erlflow_net:place -> ~p has a new input arc from ~p ~n", [ID, Source]),
            NewInputs = lists:append(Inputs, [Source]),
            place(ID, Name, NewInputs, Outputs, Tokens, Info, FormFields);
        {output, Target} ->
            io:format("erlflow_net:place -> ~p has a new output arc to ~p ~n", [ID, Target]),
            NewOutputs = lists:append(Outputs, [Target]),
            place(ID, Name, Inputs, NewOutputs, Tokens, Info, FormFields);
        {get_status, From} ->
            io:format("erlflow_net:place -> status requested by ~p ~n", [From]),
            io:format("erlflow_net:place -> status:~nID:~p~nName:~p~nInputs:~p~nOutputs:~p~nTokens:~p~nInfo:~p~nFormFields:~p~n", [ID, Name, Inputs, Outputs, Tokens, Info, FormFields]),
            From ! {status, [ID, Name, Inputs, Outputs, Tokens, Info, FormFields]},
            place(ID, Name, Inputs, Outputs, Tokens, Info, FormFields);
        {set_info, _Info} ->
            io:format("erlflow_net:place  -> set_info ~p ~n", [_Info]),
            NewInfo = lists:append([Info, _Info]),
            place(ID, Name, Inputs, Outputs, Tokens, NewInfo, FormFields);
        {set_performer, _Performer} ->
			{performer,P} = _Performer,
            io:format("erlflow_net:place  -> set_performer ~p ~n", [P]),
			PerfID = list_to_atom(P),
			PerfPID = whereis(PerfID),
			case PerfPID of
				undefined ->
					io:format("erlflow_net:place  -> registering performer ~p ~n", [PerfID]),
            		PID = spawn(erlflow_net, performer, [[]]),
					register(PerfID, PID);
				_ -> []
			end,
			PerfID ! {add_activity, ID},
            NewInfo = lists:append([Info, _Performer]),
            place(ID, Name, Inputs, Outputs, Tokens, NewInfo, FormFields);
        {transitions, From} ->
            From ! {transitions, [Outputs]},
            io:format("erlflow_net:place -> forward transitions = ~p~n", [Outputs]),
            place(ID, Name, Inputs, Outputs, Tokens, Info, FormFields);
        Other ->  
            io:format("erlflow_net:place -> Received:~p~n", [Other]),
            place(ID, Name, Inputs, Outputs, Tokens, Info, FormFields)
    end.

transition(ID, Name, Inputs, Outputs) ->
    receive
        {execute, TokenId} ->
            io:format("erlflow_net:transition -> ~p is executing token: ~p~n", [ID, TokenId]),
            lists:foreach(fun(X) -> X ! {consume_token, self(), TokenId} end, Inputs),
            transition(ID, Name, Inputs, Outputs);
        {token_consumed, TokenId} ->
            io:format("erlflow_net:transition -> ~p knows that token: ~p has been consumed~n", [ID, TokenId]),
            lists:foreach(fun(X) -> X ! {add_token, TokenId} end, Outputs), %%TODO: Actually this produce tokens in all outputs places, this is wrong accoring to theory.
            transition(ID, Name, Inputs, Outputs);
        {input, Source} ->
            io:format("erlflow_net:transition -> ~p has a new input arc from ~p ~n", [ID, Source]),
            NewInputs = lists:append(Inputs, [Source]),
            transition(ID, Name, NewInputs, Outputs);
        {output, Target} ->
            io:format("erlflow_net:transition -> ~p has a new output arc to ~p ~n", [ID, Target]),
            NewOutputs = lists:append(Outputs, [Target]),
            transition(ID, Name, Inputs, NewOutputs);
        {get_status, From} ->
            io:format("erlflow_net:transition -> status requested by ~p ~n", [From]),
            io:format("erlflow_net:transition -> status:~nName:~p~nInputs:~p~nOutputs:~p~n", [Name, Inputs, Outputs]),
            From ! {status, Name, Inputs, Outputs},
            transition(ID, Name, Inputs, Outputs);
        {places, From} ->
            From ! {places, [Outputs]},
            io:format("erlflow_net:transition -> forward places = ~p~n", [Outputs]),
            transition(ID, Name, Inputs, Outputs);
        Other ->  
            io:format("erlflow_net:transition -> received:~p~n", [Other]),
            transition(ID, Name, Inputs, Outputs)
    end.

network(Info, Places, Transitions, Fields, Participants) ->
    receive
        {add_place, ID, Name} ->
            register(ID, spawn(erlflow_net, place, [ID,Name,[],[],[],[],[]])),
            NewPlaces = lists:append(Places, [{ID, Name}]),
            io:format("erlflow_net:network -> add_place ~p ~n", [NewPlaces]),
            network(Info, NewPlaces, Transitions, Fields, Participants);
        {add_participant, ID, Name} ->
            NewParticipants = lists:append(Participants, [{ID, Name}]),
            io:format("erlflow_net:network -> add_participant ~p ~n", [NewParticipants]),
            network(Info, Places, Transitions, Fields, NewParticipants);
        {add_transition,  ID, Name} ->
            register(ID, spawn(erlflow_net, transition, [ID,Name,[],[]])),
            NewTransitions = lists:append(Transitions, [{ID, Name}]),
            io:format("erlflow_net:network -> add_place ~p ~n", [NewTransitions]),
            network(Info, Places, NewTransitions, Fields, Participants);
        {get_status, From} ->
            io:format("erlflow_net:network -> status of net is: info=~p places=~p transitions=~p participants=~p ~n", [Info, Places, Transitions, Participants]),
            From ! [lists:reverse(Info), lists:reverse(Places), lists:reverse(Transitions), lists:reverse(Participants)],
            network(Info, Places, Transitions, Fields, Participants);
        {status, State} ->
            io:format("erlflow_net:network -> status of ~w is: ~w inputs, ~w outputs, ~w tokens.~n", State),
            network(Info, Places, Transitions, Fields, Participants);
        {set_info, _Info} ->
            io:format("erlflow_net:network -> set_info ~p ~n", [_Info]),
            NewInfo = lists:flatten([Info, _Info]),
            network(NewInfo, Places, Transitions, Fields, Participants);
        {add_field, _Field} ->
            io:format("erlflow_net:network -> add_field ~p ~n", [_Field]),
            NewFields = lists:append(Fields, [_Field]),
            network(Info, Places, Transitions, NewFields, Participants);
        {get_field, From, FieldId} ->
            Field = lookup_field(FieldId, Fields),
            io:format("erlflow_net:network -> get_field response:~p from ~p~n",[Field, Fields]),
            From ! Field,
            network(Info, Places, Transitions, Fields, Participants);
        _else ->
            io:format("erlflow_net:network -> network Received:~p~n", [_else]),
            network(Info, Places, Transitions, Fields, Participants)
    end.

lookup_field(FieldId, [Head|Tail]) ->
    {Id, _,[{length, _}]} = Head,
    SEq = string:equal(Id, FieldId),
    io:format("lookup_field -> Id ~p~nlookup_field -> SEq ~p~n", [Id, SEq]),
    if
        SEq ->
            Head;
        true -> 
            lookup_field(FieldId, Tail)
    end;
lookup_field(_, []) -> [].

netsuper(Networks) ->
    receive
        {add_net, ID, Name} ->
            io:format("erlflow_net:netsuper -> add_network ~p ~n", [ID]),
            NewNetworks = lists:append(Networks, {atom_to_list(ID),Name}),
            register(ID, spawn(erlflow_net, network, [[],[],[],[],[]])),
            netsuper(NewNetworks);
        {get_status, From} ->
            From ! Networks,
            io:format("erlflow_net:netsuper -> status of net is: networks =~p ~n", [Networks]),
            netsuper(Networks);
        _else ->
            io:format("erlflow_net:netsuper -> Received:~p~n", [_else]),
            netsuper(Networks)
    end.



