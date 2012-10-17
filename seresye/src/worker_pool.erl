%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : Oct 17, 2012
%%% -------------------------------------------------------------------
-module(worker_pool).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(sync(Pid, Event),
    gen_fsm:sync_send_all_state_event(Pid, Event)).
%% --------------------------------------------------------------------
%% External exports
-export([stop/0]).

%% gen_server callbacks
-export([start_link/1,get_count/0,get_worker/0,kill_worker/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
get_worker()->
	gen_server:call(?MODULE, {get_worker}).
get_count()->
	gen_server:call(?MODULE, {get_count}).
stop()->
	gen_server:call(?MODULE, {stop}),
	(catch gen_server:call(?MODULE, stop)),
    ok.
reset({reset,BaseEngine})->
	gen_server:call(?MODULE, {reset,BaseEngine}).
kill_worker(Pid) ->
    erlang:monitor(process, Pid),
    gen_server:call(Pid, die).
%% ====================================================================
%% Server functions
%% ====================================================================
start_link(Args) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([{size,Size},{max_overflow, MaxOverflow}]) ->
	%io:format("@@@@@@@@@@@new worker~n"),
	poolboy:start_link([{name, {local, worker_engine}},
                        {worker_module, worker_engine},
                        {size, Size}, {max_overflow, MaxOverflow}]).

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_worker}, From, State) ->
    Reply = poolboy:checkout(State),
    {reply, Reply, State};

handle_call({stop}, From, State) ->
    Reply = poolboy:stop(State),
    {reply, Reply, State};

handle_call({get_count}, From, State) ->
    Reply = ?sync(State, get_avail_workers),
    {reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

