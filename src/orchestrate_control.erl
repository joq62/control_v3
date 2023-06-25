%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orchestrate_control).
 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
%% --------------------------------------------------------------------

%% External exports
-export([
	 is_wanted_state/0,
	 start_missing_deployments/0,
	 delete_deployment/1
	]).


%io:format("DeploymentId ~p~n",[{DeploymentId,?MODULE,?FUNCTION_NAME,?LINE}]),
%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment(DeploymentId)->
   ok.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_wanted_state()->

    Missing=[DeploymentId||DeploymentId<-db_deploy:get_all_id(),
			   false==vm_appl_control:is_deployed(DeploymentId)],
    case Missing of
	[]->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_missing_deployments()->
    Missing=[DeploymentId||DeploymentId<-db_deploy:get_all_id(),
		   false==vm_appl_control:is_deployed(DeploymentId)],    
    {ok,Missing}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_missing_deployments()->
    {ok,MissingDeployments}=get_missing_deployments(),
    [do_start(DeploymentId)||DeploymentId<-MissingDeployments].

do_start(DeploymentId)->
    Result=case vm_appl_control:start_vm(DeploymentId) of
	       false->
		   {error,["Couldnt start vm",DeploymentId]};
	       true->
		   case vm_appl_control:load_appl(DeploymentId) of
		       {error,Reason}->
			    {error,["Couldnt load app ",DeploymentId,Reason]};
		       ok->
			   case vm_appl_control:start_appl(DeploymentId) of
			       {error,Reason}->
				   {error,["Couldnt start app",DeploymentId,Reason]};
			       ok->
				   {ok,DeploymentId}
			   end
		   end
	   end,
    Result.
    
