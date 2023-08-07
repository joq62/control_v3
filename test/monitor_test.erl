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
-module(monitor_test).      
 
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
    test_1(),
   
    io:format("Test OK !!! ~p~n",[?MODULE]),
  
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    MonitoredNodes=control_monitor:get_monitor_nodes(),
    io:format("MonitoredNodes ~p~n",[{MonitoredNodes,?MODULE,?FUNCTION_NAME,?LINE}]),
    MonitoredProviders=control_monitor:get_monitor_providers(),
    io:format("MonitoredProviders ~p~n",[{MonitoredProviders,?MODULE,?FUNCTION_NAME,?LINE}]),
    

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
      ok.
