-module(t7).
-export([get_record_info/1]).
-compile({parse_transform, exprecs}). 
-include("../records/node.hrl").
-export_records([node]).
get_record_info(Rec)->
 Flds = '#set-'([{heap,1},{node_id,1}],'#new-'(Rec)),
Flds .

