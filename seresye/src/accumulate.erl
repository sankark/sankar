%% Author: Administrator
%% Created: Sep 26, 2012
%% Description: TODO: Add description to accumulate
-module(accumulate).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([accum/2,sum/2]).

%%
%% API Functions
%%



%%
%% Local Functions
%%


sum(FieldName,Records)->
	lists:foldl(fun(Record,Acc) -> seresye_exprecs:get_value(FieldName,Record)+Acc end, 0, Records).
accum(RecName,[])->
	accumulate(RecName);

accum(RecName,Input) ->
		  Res=accumulate(RecName,Input,[]),
	      Funs=process_funs(Res,[]),
         accumulate(RecName,Funs).
			   
process_funs([],[Acc]) ->
	io:format("~p",[Acc]),
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