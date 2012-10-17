-module(template1).

%% Record Definition



-export([get_record_info/1,new/1,set_record/2,is_recorde/1,is_recorde/2,get_value/2]).
-define(MOD,template).


get_record_info(Rec)->
 Flds = ?MOD:'#info-'(Rec),
Flds .
new(Rec)->
?MOD:'#new-'(Rec).
set_record(Vals,Rec)->
 ?MOD:'#set-'(Vals, Rec).
is_recorde(Rec)->
?MOD:'#is_record-'(Rec).

is_recorde(Name,Rec)->
?MOD:'#is_record-'(Name,Rec).

get_value(Attr,Record)->
	 ?MOD:'#get-'(Attr, Record).