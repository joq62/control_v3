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
		   StartResults=start_deployments(MissingDeployments,ClusterSpec),
		   case sd:call(etcd,etcd_lock,unlock,[Lock,TransAction],5000) of
		       {badrpc,Reason}->
			   {badrpc,Reason};
		       {error,Reason}->
			   {error,Reason};
		       ok->
			   {ok,StartResults}
		   end
	   end,
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
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,Provider}=sd:call(etcd,etcd_deployment_record,get_provider,[DeploymentRecord],5000),
    NewAcc=case control_node:start_node(DeploymentRecord,ClusterSpec) of
	       {error,Reason}->
		   ?LOG_WARNING("ERROR Failed to start Node ",[Node,Reason]),
		   [{error,Reason}|Acc];
	       ok ->
		   case control_provider:load_provider(DeploymentRecord) of
		       {error,Reason}->
			   ?LOG_WARNING("ERROR Failed to Load Provider ",[Provider,Reason]),
			   [{error,Reason}|Acc];
		       ok->
			   case control_provider:start_provider(DeploymentRecord) of
			       {error,Reason}->
				   ?LOG_WARNING("ERROR Failed to Start Provider ",[Provider,Reason]),
				   [{error,Reason}|Acc];
			       ok->
				  % ?LOG_NOTICE("Restarted Provider at Node ",[Provider , Node]),
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
    NodeIsAlive=control_node:is_alive(DeploymentRecord),
    ProviderIsAlive=control_provider:is_alive(DeploymentRecord),
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
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
