%% Author: Administrator
%% Created: Aug 23, 2012
%% Description: TODO: Add description to model_service
-module(model_service).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0,get_includes/0]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

to_list(undefined) -> [];
to_list(<<>>) -> [];
to_list({rsc_list, L}) -> L;
to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F) -> float_to_list(F).

to_atom(L) when is_list(L) -> list_to_atom(binary_to_list(list_to_binary(L))).
lib_dir(Dir) ->
	{ok, Path} = application:get_env(seresye,lib_dir),

	filename:join([Path, to_list(Dir)]).

scan(Ext,LibDir,Dir) ->	
    Hrl   = filename:join([lib_dir(LibDir),Dir,"*.hrl"]),
    Files = filelib:wildcard(Hrl) ,
    [ {to_atom(filename:basename(F,Ext)), F} ||  F <- Files ].

get_includes()->
	Hrl=scan(".hrl",src,"records"),
	lists:foldl(fun({File,_},Acc)-> Acc++lists:flatten(io_lib:format("-include(\"../records/~s.hrl\").\n",[File])) end,"",Hrl).
	
get_records()->
	Hrl=scan(".hrl",src,"records"),
	Includes=lists:foldl(fun({File,_},Acc)-> Acc++lists:flatten(io_lib:format("-include(\"../records/~s.hrl\").\n",[File])) end,"",Hrl),
	Export_Records=lists:foldl(fun({File,_},Acc)-> Acc++lists:flatten(io_lib:format("-export_records([~s]).\n",[File])) end,"",Hrl),
	{Export_Records,Includes}.

get_fields(RecordName)->
	try 
		record_info:get_record_info(RecordName) 
	catch
		error:Reason -> {'EXIT',{Reason,erlang:get_stacktrace()}}
	end.
capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.
get_record(RecordName) ->
	case get_fields(RecordName) of 
		Result -> R1=lists:foldl(fun(Field,Acc)-> Acc++lists:flatten(io_lib:format("~s=~s,",[Field,capfirst(atom_to_list(Field))])) end,"",Result),
			       "#"++atom_to_list(RecordName)++"{"++string:sub_string(R1,1,string:len(R1)-1)++"}";
		{'EXIT',Reason} -> "no record found"
	end.

compile_getrecord_template()->
	Record_Info_File   = filename:join([lib_dir(src),"rules","record_info.erl"]),
	Module   = filename:join([lib_dir(src),"rules","record_info"]),
	{ok, RulesFile} = file:open(Record_Info_File,[write]),
	{Export_Records,Includes}=get_records(),
	File_Content=lists:flatten(io_lib:format("-module(record_info).\n-export([get_record_info/1]).\n-compile({parse_transform, exprecs}). \n~s~sget_record_info(Rec)->\n Flds = '#info-'(Rec),\nFlds .",[Includes,Export_Records])),
	io:format(RulesFile,"~s",[File_Content]),
	file:close(RulesFile),
	rules_compiler:compile_rules(Module).

test()->
	application:set_env(seresye,"lib_dir","C:/ErlangTools/seresye"),
	get_record(wine).