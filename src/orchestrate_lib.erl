%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 28 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate_lib).


-include("log.api").
-include("etcd.hrl").

%% API
-export([
	 orchestrate/1,
	 is_wanted_state/0,
	 start_missing_deployments/0,
	 delete_deployment/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
orchestrate(TimeOut)->

    Result=case sd:call(etcd,db_lock,try_lock,[?OrchestrateLock,?LockTimeOut],5000) of
	       locked->
		  % ?LOG_NOTICE("locked",[]),
		   timer:sleep(TimeOut),
		   locked;
	       {ok,TransactionId} ->
		   R1=case is_wanted_state() of
			  true->
			      true;
			  false ->
			      start_missing_deployments()	
		      end,
		   %?LOG_NOTICE("R1",[R1]),
		   timer:sleep(TimeOut),
		   sd:call(etcd,db_lock,unlock,[?OrchestrateLock,TransactionId],5000),
		   R1;
	       Reason->		   
		   %?LOG_NOTICE("Error",[Reason]),
		   {error,["Un expected error ",Reason]}
	   end,
    rpc:cast(node(),orchestrate_control,orchestrate,[Result]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_missing_deployments()->
    MissingDeploymentIds=get_missing_deployments(),
    StartResult=start_deployment(MissingDeploymentIds),
    StartResult.

start_deployment(L)->
    start_deployment(L,[]).
start_deployment([],Acc)->
    Acc;
start_deployment([DeploymentId|T],Acc)->
    NewAcc=case vm_appl_control:start_deployment(DeploymentId) of
	       {ok,DeploymentId}->
		   [{ok,DeploymentId}|Acc];
	       Reason->
		   [{error,[Reason,DeploymentId]}|Acc]
	   end,
    start_deployment(T,NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment(DeploymentId)->
    vm_appl_control:delete_deployment(DeploymentId).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_wanted_state()->
    case get_missing_deployments() of
	[]->
	    true;
	_ ->
	    false
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_missing_deployments()->
    Missing=[DeploymentId||DeploymentId<-sd:call(etcd,db_deploy,get_all_id,[],5000),
			   false==vm_appl_control:is_deployed(DeploymentId)],
    Missing.
