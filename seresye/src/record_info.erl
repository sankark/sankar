-module(record_info).
-export([get_record_info/1,new/1,set_record/2,is_recorde/1]).
-compile({parse_transform, exprecs}). 
-include("records/common.hrl").
-include("records/node.hrl").
-include("records/person.hrl").
-include("records/sample.hrl").
-export_records([common]).
-export_records([node]).
-export_records([person]).
-export_records([sample]).
get_record_info(Rec)->
 Flds = '#info-'(Rec),
Flds .
new(Rec)->
'#new-'(Rec).
set_record(Vals,Rec)->
  '#set-'(Vals, Rec).
is_recorde(Rec)->
'#is_record-'(Rec).

