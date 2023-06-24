%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(orchestrate_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(DeploymentSpec,"test").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=check_deployments(),
    ok=store_deployments(),
    {ok,MissingDeployments}=get_missing_deployments(),
    ok=start_missing(MissingDeployments),
    {ok,[]}=get_missing_deployments(),


    io:format("Test OK !!! ~p~n",[?MODULE]),
  
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_missing(MissingDeployments)->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    [do_start(DeploymentId)||DeploymentId<-MissingDeployments],
    io:format("nodes ~p~n",[nodes()]),
    
    ok.

do_start(DeploymentId)->
    true=vm_appl_control:start_vm(DeploymentId),
    ok=vm_appl_control:load_appl(DeploymentId),
    ok=vm_appl_control:start_appl(DeploymentId),
    ok.
    
    
    


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_missing_deployments()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    Missing=[DeploymentId||DeploymentId<-db_deploy:get_all_id(),
		   false==vm_appl_control:is_deployed(DeploymentId)],
    io:format("Missing ~p~n",[Missing]),
    
    {ok,Missing}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
store_deployments()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %%
    DeploymentSpec=?DeploymentSpec,
    {ok,Deployment}=db_deployment_spec:read(deployment,DeploymentSpec),
    [vm_appl_control:create_deployment(DeploymentSpec,ProviderSpec,HostSpec)||{ProviderSpec,HostSpec}<-Deployment],
    io:format("init deployment ~p~n",[db_deploy:get_all_id()]),
    
    ok.
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_deployments()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    DeploymentList=db_deployment_spec:get_all_id(),
    true=lists:member(?DeploymentSpec,DeploymentList),
    {ok,Deployment}=db_deployment_spec:read(deployment,?DeploymentSpec),
    [
     {"adder","c50"},{"adder","c50"},{"adder","c50"},
     {"divi","c50"},{"divi","c50"}
    ]=lists:sort(Deployment),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok.
