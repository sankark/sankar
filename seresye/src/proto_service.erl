%% Author: Administrator
%% Created: Aug 3, 2012
%% Description: TODO: Add description to rules_compiler
-module(proto_service).
-behavior(gen_server).

-export([start_link/0, generate_model/2]).

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
generate_model(ProtoString,Model_Name) -> 
    gen_server:call(?MODULE, {generate_model,ProtoString,Model_Name}).

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
	jsontoproto:start(),
	prototojson:start(),
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
handle_call({generate_model,ProtoString,Model_Name}, _From, State) -> 
	Dest="C:/tmp/zotonic-update/deps/proto_beam/ebin/",
	Records_Dir="C:/tmp/zotonic-update/deps/seresye/src/records/",
	Proto=Dest ++ Model_Name ++ ".proto",
	{ok, ProtoFile} = file:open(Proto,[write]),
	io:format(ProtoFile,"~s",[ProtoString]),
	file:close(ProtoFile),
	code:purge(list_to_atom(Model_Name++"_pb")),
    protobuffs_compile:scan_file(Proto,[{output_ebin_dir,Dest},{output_include_dir,Records_Dir}]),
	code:load_file(list_to_atom(Model_Name++"_pb")),
	template_generator:ex1(),
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