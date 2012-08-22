-module(zotonic).
-export([zotonic/3]).
-rules([zotonic]).
zotonic(Engine0,{zotonic,test},{M,X}) when M>10 ->
Engine1=seresye_engine:assert(Engine0,test), 
seresye_engine:set_client_state(Engine1,[hi | seresye_engine:get_client_state(Engine1)]).