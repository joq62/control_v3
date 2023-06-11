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
-module(host_test).      
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(Node),
    ok=read_specs_test(Node),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllHosts=lists:sort(rpc:call(Node,db_host_spec,get_all_id,[],5000)),
    true=lists:member("c50",AllHosts),

    {
     "c50","c50","172.26.158.249",22,"joq62","festum01",[],
     "host_controller",'host_controller@c50'
    }=rpc:call(Node,db_host_spec,read,["c50"],5000),
    
    {ok,"c50"}=rpc:call(Node,db_host_spec,read,[hostname,"c50"],5000),
    {ok,"172.26.158.249"}=rpc:call(Node,db_host_spec,read,[local_ip,"c50"],5000),
    {ok,22}=rpc:call(Node,db_host_spec,read,[ssh_port,"c50"],5000),
    {ok,"joq62"}=rpc:call(Node,db_host_spec,read,[uid,"c50"],5000),
    {ok,"festum01"}=rpc:call(Node,db_host_spec,read,[passwd,"c50"],5000),
    {ok,[]}=rpc:call(Node,db_host_spec,read,[application_config,"c50"],5000),
    {ok,"host_controller"}=rpc:call(Node,db_host_spec,read,[connect_node_name,"c50"],5000),
    {ok,'host_controller@c50'}=rpc:call(Node,db_host_spec,read,[connect_node,"c50"],5000),


    {error,[eexist,"glurk",db_host_spec,_]}=rpc:call(Node,db_host_spec,read,[ssh_port,"glurk"],5000),
    {error,['Key eexists',glurk,"c50",db_host_spec,_]}=rpc:call(Node,db_host_spec,read,[glurk,"c50"],5000),
 
       ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=rpc:call(Node,dbetcd,ping,[],5000),
   
    ok.
