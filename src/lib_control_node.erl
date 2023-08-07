%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control_node).

-define(Iterations,100).
%% API
-export([start_node/2,
	 stop_node/1,
	 is_alive/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_alive(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,Dir}=sd:call(etcd,etcd_deployment_record,get_dir,[DeploymentRecord],5000),
    case is_node_started(Node) of
	false->
	    false;
	true ->
	    case rpc:call(Node,filelib,is_dir,[Dir],5000)of
		{badrpc,_Reason}->
		    false;
		false->
		    false;
		true ->
		    true
	    end
    end.
								 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_node(DeploymentRecord,ClusterSpec)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,Dir}=sd:call(etcd,etcd_deployment_record,get_dir,[DeploymentRecord],5000),
    {ok,HostSpec}=sd:call(etcd,etcd_deployment_record,get_host,[DeploymentRecord],5000),
    {ok,NodeName}=sd:call(etcd,etcd_deployment_record,get_node_name,[DeploymentRecord],5000),
    {ok,CookieStr}=sd:call(etcd,etcd_cluster,get_cookie_str,[ClusterSpec],5000),
    %% Clean up before start new node 
    case stop_node(DeploymentRecord) of
	{error,Reason}->
	    {error,["Failed to clean up when starting new node",?MODULE,?LINE,Reason,DeploymentRecord]};
	ok->	
	    %% Create new node
	    case create_dir(Dir,HostSpec) of
		{error,Reason}->
		    {error,["Failed to create dir when starting new node",?MODULE,?LINE,Reason,DeploymentRecord]};
		ok->
		    create_vm(NodeName,Node,CookieStr,HostSpec)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_node(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,Dir}=sd:call(etcd,etcd_deployment_record,get_dir,[DeploymentRecord],5000),
    {ok,HostSpec}=sd:call(etcd,etcd_deployment_record,get_host,[DeploymentRecord],5000),
    del_dir(Dir,HostSpec),
    case kill_vm(Node) of
	false->
	    {error,["Failed to stop node ",DeploymentRecord,?MODULE,?LINE]};
	true->
	    ok
    end.
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
del_dir(Dir,HostSpec)->
    {ok,Ip}=sd:call(etcd,etcd_host,get_ip,[HostSpec],5000),
    {ok,Port}=sd:call(etcd,etcd_host,get_port,[HostSpec],5000),
    {ok,Uid}=sd:call(etcd,etcd_host,get_user,[HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,etcd_host,get_passwd,[HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="pwd",
    {ok,[HomeDir]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    ProviderDir=filename:join(HomeDir,Dir),
    DelDir=ssh_server:send_msg(Ip,Port,Uid,Pwd,"rm -r "++ProviderDir,TimeOut),
    case DelDir of
	{ok,[]}->
	    ok;
	Error->
	    {error,["Failed to delete Dir",?MODULE,?LINE,Dir,Error]}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_dir(Dir,HostSpec)->
    {ok,Ip}=sd:call(etcd,etcd_host,get_ip,[HostSpec],5000),
    {ok,Port}=sd:call(etcd,etcd_host,get_port,[HostSpec],5000),
    {ok,Uid}=sd:call(etcd,etcd_host,get_user,[HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,etcd_host,get_passwd,[HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="pwd",
    {ok,[HomeDir]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    ProviderDir=filename:join(HomeDir,Dir),
    MakeDir=ssh_server:send_msg(Ip,Port,Uid,Pwd,"mkdir "++ProviderDir,TimeOut),
    case MakeDir of
	{ok,[]}->
	    ok;
	Error->
	   {error,["Failed to make Dir",?MODULE,?LINE,Dir,Error]} 
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
kill_vm(Node)->
    rpc:call(Node,init,stop,[],5000),
    is_node_stopped(Node).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_vm(NodeName,Node,CookieStr,HostSpec)->
    {ok,Ip}=sd:call(etcd,etcd_host,get_ip,[HostSpec],5000),
    {ok,Port}=sd:call(etcd,etcd_host,get_port,[HostSpec],5000),
    {ok,Uid}=sd:call(etcd,etcd_host,get_user,[HostSpec],5000),
    {ok,Pwd}=sd:call(etcd,etcd_host,get_passwd,[HostSpec],5000),
    TimeOut=5000,
    LinuxCmd="erl -sname "++NodeName++" "++"-setcookie "++CookieStr++" "++"-detached",
    ErlStart=ssh_server:send_msg(Ip,Port,Uid,Pwd,LinuxCmd,TimeOut),
    case is_node_started(Node) of
	false->
	    {error,["Failed to start Node,ErlStart ",Node,ErlStart,?MODULE,?LINE]};
	true->
	    ok
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_node_started(Node)->
    is_node_started(?Iterations,Node,false).

is_node_started(_N,_Node,true)->
    true;
is_node_started(0,_Node,Boolean) ->
    Boolean;
is_node_started(N,Node,_) ->
  %  io:format(" ~p~n",[{N,Node,erlang:get_cookie(),?MODULE,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(30),
		    false;
		pong->
		    true
	    end,
    is_node_started(N-1,Node,Boolean).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_node_stopped(Node)->
    is_node_stopped(?Iterations,Node,false).

is_node_stopped(_N,_Node,true)->
    true;
is_node_stopped(0,_Node,Boolean) ->
    Boolean;
is_node_stopped(N,Node,_) ->
    Boolean=case net_adm:ping(Node) of
		pong->
		    timer:sleep(500),
		    false;
		pang->
		    true
	    end,
    is_node_stopped(N-1,Node,Boolean).
