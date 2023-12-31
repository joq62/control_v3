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
-module(all).      
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(RecordTest,{deployment_record,"a_1",'a_1@c50',etcd,"a_1.provider_dir","etcd","c50"}).
-define(ClusterSpec,"test_c50_1").
-define(DeploymentSpec,"test_c50").
-define(HostSpec,"c50").
-define(CookieStr,"a").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec])->
   
    ok=setup(ClusterSpec),
 %   ok=monitor_test:start(),
  %  ok=create_del_node_and_app(),
   % io:format("Test OK !!! ~p~n",[?MODULE]),
    io:format("OK there you go !!! ~p~n",[?MODULE]),
 %   timer:sleep(2000),
  %  init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_del_node_and_app()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),    
    {ok,ClusterSpec}=etcd_paas_config:get_cluster_spec(),
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[?RecordTest],5000),
    {ok,Dir}=sd:call(etcd,etcd_deployment_record,get_dir,[?RecordTest],5000),
    ok=control_node:stop_node(?RecordTest),
    false=control_node:is_alive(?RecordTest),
    false=control_provider:is_alive(?RecordTest),
    %% Create Node
    ok=control_node:start_node(?RecordTest,ClusterSpec),
    true=control_node:is_alive(?RecordTest),
    pong=net_adm:ping(Node),
    
    %% load start provider
    false=control_provider:is_alive(?RecordTest),
    ok=control_provider:load_provider(?RecordTest),
    ok=control_provider:start_provider(?RecordTest),
    true=control_provider:is_alive(?RecordTest),
    io:format(" applications ~p~n",[{?MODULE,?LINE,rpc:call(Node,application,which_applications,[],5000)}]),
    ok=control_provider:stop_provider(?RecordTest),
    ok=control_provider:unload_provider(?RecordTest),
    false=control_provider:is_alive(?RecordTest),
    %% Stop node
    ok=control_node:stop_node(?RecordTest),
    false=control_node:is_alive(?RecordTest),
    io:format("Debug  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),    
    pang=net_adm:ping(Node),
        
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,ClusterSpec}]),
    ok=application:start(log),
    pong=log:ping(),
    %
    ok=application:start(etcd),
    pong=etcd:ping(),
   
    %% Simulate sys_boot
        
    ok=etcd_paas_config:create(ClusterSpec,lock_test),
    ok=etcd_lock:create(lock_test),
    {ok,DeploymentRecords}=etcd_deployment_record:create_records(ClusterSpec),
    io:format("DeploymentRecords ~p~n",[{?MODULE,?FUNCTION_NAME,DeploymentRecords}]),
    ok=etcd_cluster:set_deployment_records(DeploymentRecords,ClusterSpec),
    
    % kill nodes
    KilledNodes=kill_nodes(DeploymentRecords,[]),
    io:format("KilledNodes ~p~n",[{?MODULE,?FUNCTION_NAME,KilledNodes}]),

    %% End  Simulate sys_boot

    ok=application:start(control),
    pong=ssh_server:ping(),
    pong=control:ping(),
    ok.

kill_nodes([],Acc)->
    Acc;
kill_nodes([DeploymentRecord|T],Acc)->
    {ok,Node}=etcd_deployment_record:get_node(DeploymentRecord),
    R=rpc:call(Node,init,stop,[],5000),
    kill_nodes(T,[{R,Node}|Acc]).
