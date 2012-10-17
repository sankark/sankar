-module(record_info).
-export([get_record_info/1,new/1,set_record/2,is_recorde/1]).
-compile({parse_transform, exprecs}). 
-include("records/proto_template_pb.hrl").
-export_records([proto_template_pb]).
get_record_info(Rec)->
 Flds = '#info-'(Rec),
Flds .
new(Rec)->
'#new-'(Rec).
set_record(Vals,Rec)->
  '#set-'(Vals, Rec).
is_recorde(Rec)->
'#is_record-'(Rec).