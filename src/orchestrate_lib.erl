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
    ?LOG_NOTICE("start ",[]),
    timer:sleep(TimeOut),
    Result=case is_wanted_state() of
	       true->
		   true;
	       false ->
		   start_missing_deployments()		   
	   end,
    ?LOG_NOTICE("Result ",[Result]),
    rpc:cast(node(),orchestrate_control,orchestrate,[Result]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_missing_deployments()->
    [rpc:call(node(),vm_appl_control,start_deployment,[DeploymentId]
,2*5000)||DeploymentId<-get_missing_deployments()].
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
