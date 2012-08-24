%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
	set_path(),
    seresye_sup:start_link(),
	seresye:start(defaultengine),
	rules_compiler:start_link(),
	rules_compiler:start(rulescompiler).
	

stop(_State) ->
    ok.

set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	application:set_env(seresye, lib_dir, Path).