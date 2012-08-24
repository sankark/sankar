-module(record_info).
-export([get_record_info/1]).
-compile({parse_transform, exprecs}). 
-include("../records/node.hrl").
-include("../records/nodes.hrl").
-include("../records/ontology.hrl").
-include("../records/test.hrl").
-include("../records/wine.hrl").
-export_records([node]).
-export_records([nodes]).
-export_records([ontology]).
-export_records([test]).
-export_records([wine]).
get_record_info(Rec)->
 Flds = '#info-'(Rec),
Flds .