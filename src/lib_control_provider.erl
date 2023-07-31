%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control_provider).

%% API
-export([
	 load_provider/1,
	 start_provider/1,
	 stop_provider/1,
	 unload_provider/1,
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
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
    case rpc:call(Node,App,ping,[],5000) of
	{badrpc,_Reason}->
	    false;
	pong->
	    true
    end.
								 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_provider(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
    {ok,Provider}=sd:call(etcd,etcd_deployment_record,get_provider,[DeploymentRecord],5000),
    {ok,Dir}=sd:call(etcd,etcd_deployment_record,get_dir,[DeploymentRecord],5000),
    {ok,GitPath}=sd:call(etcd,etcd_provider,get_git_path,[Provider],5000),
    
    case rpc:call(Node,os,cmd,["git clone "++GitPath++" "++Dir],3*5000) of
	{badrpc,Reason}->
	    {error,["Failed to git clone ",?MODULE,?LINE,Reason]};
	GitResult->
	    Ebin=filename:join([Dir,"ebin"]),
	    case rpc:call(Node,code,add_patha,[Ebin],5000) of
		{badrpc,Reason}->
		    {error,["Failed to set path  ",?MODULE,?LINE,Dir,Reason,GitResult]};
		{error, bad_directory}->
		    {error,["Failed to  set path ",?MODULE,?LINE, bad_directory,Ebin]};
		true->
		    case rpc:call(Node,application,load,[App],5000) of
			{badrpc,Reason}->
			    {error,["Failed to load  ",?MODULE,?LINE,Reason]};
			{error,Reason}->
			    {error,["Failed to load  ",?MODULE,?LINE,Reason]};
			ok->
			    ok
		    end
	    end
    end.
			
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_provider(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
      
    case rpc:call(Node,application,start,[App],5000) of
	{badrpc,Reason}->
	    {error,["Failed to start  ",?MODULE,?LINE,Reason]};
	{error,Reason}->
	    {error,["Failed to tart  ",?MODULE,?LINE,Reason]};
	ok->
	    ok
    end.
			    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_provider(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
      
    case rpc:call(Node,application,stop,[App],5000) of
	{badrpc,Reason}->
	    {error,["Failed to stop  ",?MODULE,?LINE,Reason]};
	{error,Reason}->
	    {error,["Failed to stop  ",?MODULE,?LINE,Reason]};
	ok->
	    ok
    end.
			    
			    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload_provider(DeploymentRecord)->
    {ok,Node}=sd:call(etcd,etcd_deployment_record,get_node,[DeploymentRecord],5000),
    {ok,App}=sd:call(etcd,etcd_deployment_record,get_app,[DeploymentRecord],5000),
      
    case rpc:call(Node,application,unload,[App],5000) of
	{badrpc,Reason}->
	    {error,["Failed to unload ",?MODULE,?LINE,Reason]};
	{error,Reason}->
	    {error,["Failed to unload  ",?MODULE,?LINE,Reason]};
	ok->
	    ok
    end.
			    

