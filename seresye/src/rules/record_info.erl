-module(record_info).
-export([get_record_info/1]).
-compile({parse_transform, exprecs}). 
-include("../records/ontology.hrl").
-include("../records/wine.hrl").
-export_records([ontology]).
-export_records([wine]).
get_record_info(Rec)->
 Flds = '#info-'(Rec),
Flds .