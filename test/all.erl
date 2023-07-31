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


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec])->
   
    ok=setup(ClusterSpec),

    
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=application:start(log),
    pong=log:ping(),
    %
    ok=application:start(etcd),
    pong=etcd:ping(),
   
    %% Simulate sys_boot
    Node=node(),
    ok=etcd_lock:create(ClusterSpec),
    ok=etcd_cluster_to_deploy:create(ClusterSpec,node()),
   
    {ok,DeploymentRecords}=etcd_deployment_record:create_records(ClusterSpec),
    ok=etcd_cluster:set_deployment_records(DeploymentRecords,ClusterSpec),
   
    %% End  Simulate sys_boot

    ok=application:start(control),
    pong=ssh_server:ping(),
    pong=control:ping(),
    ok.
