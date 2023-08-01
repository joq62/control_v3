%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control_orchestrate).
-include("log.api").

%% API
-export([
	 start/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(Interval)->
    timer:sleep(Interval),
    {ok,Lock}=sd:call(etcd,etcd_paas_config,get_lock,[],5000),
    LockTimeOut=2*Interval,
    Result=case sd:call(etcd,etcd_lock,try_lock,[Lock,LockTimeOut],5000) of
	       {badrpc,Reason}->
		   {badrpc,Reason};
	       locked->
		   locked;
	       {ok,TransAction}->
		   {ok,ClusterSpec}=sd:call(etcd,etcd_paas_config,get_cluster_spec,[],5000),
		   MissingDeployments=missing_deployments(ClusterSpec),
		   ?LOG_NOTICE("Debug: MissingDeployments ",[node(),MissingDeployments]),
		   StartResults=start_deployments(MissingDeployments,ClusterSpec),
		   case sd:call(etcd,etcd_lock,unlock,[Lock,TransAction],5000) of
		       {badrpc,Reason}->
			   {badrpc,Reason};
		       {error,Reason}->
			   {error,Reason};
		       ok->
			   StartResults
		   end
	   end,
    ?LOG_NOTICE("Debug ",[node(),Result]),
    rpc:cast(node(),control_orchestrate,result,[Result]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_deployments(MissingDeployments,ClusterSpec)->
    start_deployments(MissingDeployments,ClusterSpec,[]).

start_deployments([],_ClusterSpec,Acc)->
    Acc;
start_deployments([DeploymentRecord|T],ClusterSpec,Acc) ->
    NewAcc=case control_node:start_node(DeploymentRecord,ClusterSpec) of
	       {error,Reason}->
		   [{error,Reason}|Acc];
	       ok ->
		   case control_provider:load_provider(DeploymentRecord) of
		       {error,Reason}->
			   [{error,Reason}|Acc];
		       ok->
			   case control_provider:start_provider(DeploymentRecord) of
			       {error,Reason}->
				   [{error,Reason}|Acc];
			       ok->
				   [{ok,DeploymentRecord}|Acc]
			   end
		   end
	   end,
    start_deployments(T,ClusterSpec,NewAcc).
			   

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
missing_deployments(ClusterSpec)->
    {ok,DeploymentRecords}=sd:call(etcd,etcd_cluster,get_deployment_records,[ClusterSpec],5000),
    missing_deployments(DeploymentRecords,[]).
    


missing_deployments([],Acc)->
    Acc;
missing_deployments([DeploymentRecord|T],Acc)->
    NodeIsAlive=sd:call(etcd,control_node,is_alive,[DeploymentRecord],1*5000),
    ProviderIsAlive=sd:call(etcd,control_provider,is_alive,[DeploymentRecord],1*5000),
    ?LOG_NOTICE("NodeIsAlive,ProviderIsAlive,DeploymentRecord ",[NodeIsAlive,ProviderIsAlive,DeploymentRecord]),
    NewAcc=case {NodeIsAlive,ProviderIsAlive} of
	       {true,true}->
		   Acc;
	       _->
		   [DeploymentRecord|Acc]
	   end,
    missing_deployments(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
