%% Author: Administrator
%% Created: Sep 20, 2012
%% Description: TODO: Add description to rfc4627_exprecs
-module(seresye_exprecs).
-compile({parse_transform, exprecs}). 
-include("addressbook_pb.hrl").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%



%%
%% Local Functions
%%

get_record_info(Rec)->
 Flds = '#info-'(Rec),
Flds .
new(Rec)->
'#new-'(Rec).
set_record(Vals,Rec)->
  '#set-'(Vals, Rec).
is_recorde(Rec)->
'#is_record-'(Rec).

is_recorde(Name,Rec)->
'#is_record-'(Name,Rec).

get_value(Attr,Record)->
	 '#get-'(Attr, Record).

