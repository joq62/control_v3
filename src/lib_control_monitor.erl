%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control_monitor).
-include("log.api").

%% API
-export([
	 set_monitor_node/1,
	 set_monitor_nodes/1,
	 set_monitor_provider/1,
	 set_monitor_providers/1,
	 
	 start/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ClusterSpec)-> 
    {ok,DeploymentRecords}=sd:call(etcd,etcd_cluster,get_deployment_records,[ClusterSpec],5000),
    {ok,DeploymentRecords}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_monitor_node(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    Flag=true,
    true=erlang:monitor_node(Node, Flag),
    {ok,DeploymentRecord}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_monitor_nodes(ClusterSpec)->
    {ok,DeploymentRecords}=sd:call(etcd,etcd_cluster,get_deployment_records,[ClusterSpec],5000),
    set_monitor_nodes(DeploymentRecords,[]).
set_monitor_nodes([],Acc)->
    Acc;
set_monitor_nodes([DeploymentRecord|T],Acc)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    Flag=true,
    true=erlang:monitor_node(Node, Flag),
    set_monitor_nodes(T,[{ok,DeploymentRecord}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_monitor_provider(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
    MonitorRef=erlang:monitor(process, {App,Node}),
    {ok,MonitorRef,DeploymentRecord}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_monitor_providers(ClusterSpec)->
    {ok,DeploymentRecords}=sd:call(etcd,etcd_cluster,get_deployment_records,[ClusterSpec],5000),
    set_monitor_providers(DeploymentRecords,[]).   
set_monitor_providers([],Acc)->
    Acc;
set_monitor_providers([DeploymentRecord|T],Acc)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
    MonitorRef=erlang:monitor(process, {App,Node}),
    set_monitor_providers(T,[{ok,MonitorRef,DeploymentRecord}|Acc]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
