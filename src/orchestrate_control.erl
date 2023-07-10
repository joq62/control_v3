%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate_control).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
%% --------------------------------------------------------------------

-define(OrchestrateTimeOut,30*1000).

%% API
-export([
	 is_wanted_state/0,
	 start_missing_deployments/0,
	 delete_deployment/1,
	 orchestrate/1,
	 ping/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).
%io:format("DeploymentId ~p~n",[{DeploymentId,?MODULE,?FUNCTION_NAME,?LINE}]),

%%%===================================================================
%%% API
%%%===================================================================

is_wanted_state()->
    gen_server:call(?SERVER, {is_wanted_state},infinity).    

start_missing_deployments()->
    gen_server:call(?SERVER, {start_missing_deployments},infinity).    

delete_deployment(DeploymentId)->
    gen_server:call(?SERVER, {delete_deployment,DeploymentId},infinity).    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchestrate(Result)->
    gen_server:cast(?SERVER, {orchestrate,Result}). 

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    ?LOG_NOTICE("Server started",[]),
    rpc:cast(node(),orchestrate_lib,orchestrate,[?OrchestrateTimeOut]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_call({is_wanted_state}, _From, State) ->
    Reply=rpc:call(node(),orchestrate_lib,is_wanted_state,[],2*5000),
    {reply, Reply, State};

handle_call({start_missing_deployments}, _From, State) ->
    Reply=rpc:call(node(),orchestrate_lib,start_missing_deployments,[],10*5000),
    {reply, Reply, State};

handle_call({delete_deployment,DeploymentId}, _From, State) ->
    Reply=rpc:call(node(),orchestrate_lib,delete_deployment,[DeploymentId],5000),
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply ={error,["Unmatched signal ",Request, From,?MODULE,?LINE]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_cast({orchestrate,Result}, State) ->
    ?LOG_NOTICE("Result ",[Result]),
 %   io:format("Result ~p~n",[{Result,?MODULE,?LINE}]),
    io:format("is_wanted_state ~p~n",[{rpc:call(node(),orchestrate_lib,is_wanted_state,[],2*5000),?MODULE,?LINE}]),
    
    rpc:cast(node(),orchestrate_lib,orchestrate,[?OrchestrateTimeOut]),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

		   
