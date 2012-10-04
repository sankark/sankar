%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module (seresyet_sample2).
-export ([rule/4, rule1/3, rule2/4, rule3/3, start/0,test/0]).
-compile({parse_transform, exprecs}). 
-record (sample_record, { a = nil, b}).
-export_records([sample_record]).
-include_lib("eunit/include/eunit.hrl").


rule (Engine, {hello, world}, {ciao, X}, {X, 10}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule | seresye_engine:get_client_state(Engine)]).

rule1 (Engine0, {hello, world}, {ciao, _X} = F) ->
    Engine1 = seresye_engine:assert (Engine0, {test}),
    Engine2 = seresye_engine:retract (Engine1, F),
    seresye_engine:set_client_state(Engine2,
                                    [rule1a | seresye_engine:get_client_state(Engine2)]);
rule1 (Engine, {hello, world}, {test}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule1b | seresye_engine:get_client_state(Engine)]).

rule2 (Engine, {hello, world}, #sample_record { a = Z }, {mondo, Z}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule2 | seresye_engine:get_client_state(Engine)]).

rule3 (Engine, {hello, [_H|T]}, {test, T}) ->
    seresye_engine:set_client_state(Engine,
                                    [rule3 | seresye_engine:get_client_state(Engine)]).


start () ->

     Engine0 = seresye_engine:add_rule (seresye_engine:new([]), {?MODULE, rule2}),
    Engine1 = seresye_engine:assert (Engine0, [[{ciao, mondo}, {mondo, 20}],
                                               {hello, world},
                                               {ok, world},
                                               #sample_record { a = 10, b = 50}]),
	
	   Input=[{b,gt,10}],
		  F = accum(sample_record,Input),
		  FilteredList=seresye_engine:query_kb(Engine1,F),
	   io:format("Fl~p",[sum(b,FilteredList)]),
	   
	   
     Engine2 = seresye_engine:add_rule (Engine1, {?MODULE, rule3}),
    Engine3 = seresye_engine:assert (Engine2, [{hello, [ciao, mondo]},
                                               {test, ciao},
                                               {test, [ciao]},
                                               {test, [mondo]},
                                               {hello, [ciao, mondo, world]},
                                               {test, [mondo, world]}]),

    Engine4 = seresye_engine:add_rules (Engine3, [{?MODULE, rule},
                                                  {?MODULE, rule1}]),

    Engine5 = seresye_engine:retract (Engine4, {test}),
    State = seresye_engine:get_client_state(seresye_engine:assert (Engine5, {test})).


parse_file(File, Opts) ->
    Cwd = ".",
    Dir = filename:dirname(File),
    IncludePath = [Cwd,Dir|inc_paths(Opts)],
    case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
        {ok,Forms} ->
            record_attrs(Forms);
        Error ->
            Error
    end.
pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [P || {i,P} <- Opts, is_list(P)].

record_attrs(Forms) ->
    [A || A = {attribute,_,record,_D} <- Forms].

sum(FieldName,Records)->
	lists:foldl(fun(Record,Acc) -> seresye_exprecs:get_value(FieldName,Record)+Acc end, 0, Records).
	
test()->
%% 	F1=fun(X)->
%% 			  seresye_exprecs:get_value(a,X) == "sankar" end,
%% 	F2=fun(X)->
%% 			  seresye_exprecs:get_value(b,X) > 1 end,
%% 	F3=fun(X)->
%% 			  seresye_exprecs:get_value(b,X) > 11 end,
%% 	Funs=[F1,{'and'},F2,{'and'},F3],
%% 	F=process_funs(Funs,[]),

	%% Accumulate test
%% 		Input=[{b,gt,10},{'and'},{a,lt,10}],
%% 		Res=accumulate(sample_record,Input,[]),
%% 		F=process_funs(Res,[]),
%% 	io:format("~p",[F(#sample_record{a=10,b=20})]).
RAs=parse_file("C:/tmp/sankar/zotonic-update/deps/erlang_protobuffs/addressbook_pb.hrl",[]),
Recs = [Name || {attribute,_,_,{Name,_}} <- RAs],
io:format("~p",[Recs]).

accum(RecName,[])->
	accumulate(RecName);

accum(RecName,Input) ->
		  Res=accumulate(RecName,Input,[]),
	      Funs=process_funs(Res,[]),
         accumulate(RecName,Funs).
			   
process_funs([],[Acc]) ->
	Acc;

process_funs([Fun|Rest],[])->
	process_funs(Rest,[Fun]);
process_funs([Oper|[Fun|Rest]],[Acc])->
	X=case Oper of 
		{'and'} -> fun(X)->Acc(X) and Fun(X) end;
		{'or'} -> fun(X)->Acc(X) or Fun(X) end
	
	end,

	%io:format("~p",[X]),
		process_funs(Rest,[X]).

accumulate(_Y,[],Acc)->
	lists:flatten(Acc);
accumulate(Y,[H|Rest],[])->
	accumulate(Y,Rest,[accumulate(Y,H)]);
accumulate(Y,[Oper|[H|T]],Acc)->
	accumulate(Y,T,[Acc|[Oper,accumulate(Y,H)]]).


accumulate(_Y,{Attr,gt,Value})->
	F=fun(X)->
			  seresye_exprecs:get_value(Attr,X) > Value end,
	F;
accumulate(_Y,{Attr,lt,Value})->
	F=fun(X)->
			  seresye_exprecs:get_value(Attr,X) < Value end,
	F;

accumulate(RecordName,Funtion)->
	 F=fun(X) ->
	       case is_record(X, RecordName) of
			   true -> Funtion(X) ;
			   false -> false
		   end
	   end,
    F.
accumulate(Y) ->
			 F=fun(X) ->
	       is_record(X, Y) end,
    F.