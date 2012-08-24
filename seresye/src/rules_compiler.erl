%% Author: Administrator
%% Created: Aug 3, 2012
%% Description: TODO: Add description to rules_compiler
-module(rules_compiler).
-behavior(gen_server).

-export([start_link/0, compile_rules/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3,start/1]).
-record(state, {sup}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) when is_atom(Name) -> 
    gen_server:start_link({local, Name}, ?MODULE, [], []). 


start(Name) ->
    seresye_sup:start_compiler(Name).
%%--------------------------------------------------------------------
%% Function: print_state() -> ok
%% Description: Prints the current state to std output
%%--------------------------------------------------------------------
compile_rules(Module) -> 
    gen_server:call(?MODULE, {compile_rules,Module}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) -> 
    {ok, #state{sup=[]}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({compile_rules,Module}, _From, State) -> 
	Dest="C:/ErlangTools/seresye/ebin/",
	  FileName = filename:basename(Module),
    DirName = filename:dirname(Module),
    %io:format("state: ~p~n", [code:get_path()]),
	{ok,ModName,Binary}=compile:file(Module,[binary,debug_info]),
	{ok, File}=file:open(Dest ++ FileName ++ ".beam",[write]),
	file:write(File,Binary),
	%io:format(File, "~p", [Binary]),
    file:close(File),
	code:purge(rules_compiler),
	code:load_file(rules_compiler),
    {reply, ok,State};

handle_call(_Call, _From, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Cast, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.