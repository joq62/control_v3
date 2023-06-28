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
-define(Home,"/home/joq62").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=check_deployments(),
    ok=store_deployments(),
%    false=orchestrate_control:is_wanted_state(),
%    ok=start_missing(),
%    true=orchestrate_control:is_wanted_state(),
%    ok=sd_check(),

%    ok=delete_deployments(),
%    []=db_deploy:get_all_id(),
%    true=orchestrate_control:is_wanted_state(),

    %
%    ok=store_deployments(),
%    false=orchestrate_control:is_wanted_state(),
%    ok=start_missing(),
%    true=orchestrate_control:is_wanted_state(),
%    ok=sd_check(),

    ok=monkey_test:start(),
    io:format("Test OK !!! ~p~n",[?MODULE]),
  
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployments()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllDeployments=db_deploy:get_all_id(),
    DeleteR=[vm_appl_control:delete_deployment(DeploymentId)||DeploymentId<-AllDeployments],
    io:format("DeleteR  ~p~n",[DeleteR]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
sd_check()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    [N1|_]=sd:get_node(adder),
     io:format("N1 dir ~p~n",[rpc:call(N1,file,get_cwd,[],5000)]),
    3=erlang:length(sd:get_node(adder)),
    2=erlang:length(sd:get_node(divi)),

    42=sd:call(adder,adder,add,[20,22],5000),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_missing()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    StartR=orchestrate_control:start_missing_deployments(),
    
    io:format("StartR ~p~n",[StartR]),
    ok.

   
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
     {"adder","c200"},{"adder","c201"},{"adder","c50"},{"adder","c50"},{"adder","c50"},
     {"divi","c200"},{"divi","c201"},{"divi","c50"},{"divi","c50"}
    ]=lists:sort(Deployment),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,F}=file:list_dir(?Home),
    Files=[filename:join(?Home,File)||File<-F,
		 ".provider_dir"==filename:extension(File)],
    case Files of
	[]->
	    ok;
	_->
	    [ok|_]=[file:del_dir_r(File)||File<-Files]
    end,

    ok.
