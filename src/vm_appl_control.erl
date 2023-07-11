%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(vm_appl_control).
 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.api").
%% --------------------------------------------------------------------

%% External exports
-export([
	 create_deployment/3,
	 create_deployment/4,
	 create_deployment/5,
	 delete_deployment/1,
	 start_deployment/1,
	 restart_deployment/1,
	 is_vm_started/1,
	
	 start_vm/1,
	 stop_vm/1,

%	 create_dir/1,
	 load_appl/1,
%	 is_appl_loaded/1,
	 start_appl/1,
%	 is_appl_started/1,
	 stop_appl/1,
	 unload_appl/1,
	 delete_dir/1,
	 
	 check_stopped_node/1,
	 check_started_node/1
	 ]).

-export([
	 is_deployed/1,
	 unique_node_name/2,
	 deployment_id/1,
	 dir/1
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
start_appl(DeploymentId)->
    {ok,ProviderSpec}=sd:call(etcd,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,App}=sd:call(etcd,db_provider_spec,read,[app,ProviderSpec],5000),
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    rpc:call(Node,application,start,[App],3*5000).
	    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_appl(DeploymentId)->
    {ok,ProviderSpec}=sd:call(etcd,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,App}=sd:call(etcd,db_provider_spec,read,[app,ProviderSpec],5000),
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    rpc:call(Node,application,stop,[App],3*5000).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_appl(DeploymentId)->
    {ok,ProviderSpec}=sd:call(etcd,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,GitPath}=sd:call(etcd,db_provider_spec,read,[git_path,ProviderSpec],5000),
    {ok,App}=sd:call(etcd,db_provider_spec,read,[app,ProviderSpec],5000),
    {ok,Dir}=sd:call(etcd,db_deploy,read,[dir,DeploymentId],5000),
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),

    %% Create dir and clone
    rpc:call(Node,file,del_dir_r,[Dir],5000), 
    ok=rpc:call(Node,file,make_dir,[Dir],5000),
    CloneCmd="git clone "++GitPath++" "++Dir,
    CloneResult=rpc:call(Node,os,cmd,[CloneCmd],2*5000),
    true=?LOG_NOTICE("CloneResult",[CloneResult]),
    Ebin=filename:join(Dir,"ebin"),
    true=rpc:call(Node,code,add_patha,[Ebin],5000),
    ok=rpc:call(Node,application,load,[App],5000),
    
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unload_appl(DeploymentId)->
    {ok,ProviderSpec}=sd:call(etcd,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,HostSpec}=sd:call(etcd,db_deploy,read,[host_spec,DeploymentId],5000),
    {ok,Dir}=sd:call(etcd,db_deploy,read,[dir,DeploymentId],5000),
    {ok,App}=sd:call(etcd,db_provider_spec,read,[app,ProviderSpec],5000),
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),

    %% stop unload appl and delete dir
    ok=rpc:call(Node,application,unload,[App],5000),
    ssh_server:send_msg(HostSpec,"rm -rf "++Dir,5000),
    
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment(DeploymentSpec,ProviderSpec,HostSpec)->
    Type="provider",
    create_deployment(DeploymentSpec,ProviderSpec,HostSpec,Type).
create_deployment(DeploymentSpec,ProviderSpec,HostSpec,Type)->
    {ok,AppName}=sd:call(etcd,db_provider_spec,read,[app_name,ProviderSpec],5000),
    {ok,Unique}=unique_node_name(AppName,Type),
    create_deployment(DeploymentSpec,ProviderSpec,HostSpec,Type,Unique).

create_deployment(DeploymentSpec,ProviderSpec,HostSpec,_Type,Unique)->
    DeploymentId=Unique,
    NodeName=Unique,
    Dir=Unique++"."++"provider_dir",
    {ok,HostName}=sd:call(etcd,db_host_spec,read,[hostname,HostSpec],5000),
    Node=list_to_atom(Unique++"@"++HostName),
    CreationTime={date(),time()},
    {atomic,ok}=sd:call(etcd,db_deploy,create,[DeploymentId,DeploymentSpec,ProviderSpec,NodeName,Dir,Node,HostSpec,CreationTime],5000),
    {ok,DeploymentId}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
restart_deployment(DeploymentId)->
    stop_vm(DeploymentId),
    delete_dir(DeploymentId),  
    start_deployment(DeploymentId).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_deployment(DeploymentId)->
    ?LOG_NOTICE("DeploymentId %%%%%%%%%%%%%%%%%%%%% ",[DeploymentId]),
    Result=case start_vm(DeploymentId) of
	       false->
		   {error,["Couldnt start vm",DeploymentId]};
	       true->
		   case load_appl(DeploymentId) of
		       {error,Reason}->
			    {error,["Couldnt load app ",DeploymentId,Reason]};
		       ok->
			   case start_appl(DeploymentId) of
			       {error,Reason}->
				   {error,["Couldnt start app",DeploymentId,Reason]};
			       ok->
				   ?LOG_NOTICE("Deployment started",[DeploymentId]),
				   {ok,DeploymentId}
			   end
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment(DeploymentId)->
    stop_vm(DeploymentId),
    delete_dir(DeploymentId),    
    {atomic,ok}=sd:call(etcd,db_deploy,delete,[DeploymentId],5000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_vm(DeploymentId)->
 
    %% Ensure Node  stopped
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    rpc:call(Node,init,stop,[],5000),
    true=check_stopped_node(Node),
    %% ssh start vm    
    CookieStr=atom_to_list(erlang:get_cookie()),
    {ok,NodeName}=sd:call(etcd,db_deploy,read,[node_name,DeploymentId],5000),
    {ok,HostSpec}=sd:call(etcd,db_deploy,read,[host_spec,DeploymentId],5000),
    LinuxCmd="erl -sname "++NodeName++" "++" -setcookie "++CookieStr++" "++" -detached ",
    TimeOut=2*5000,
    {ok,[]}=ssh_server:send_msg(HostSpec,LinuxCmd,TimeOut),
    case check_started_node(Node) of
	true->
	    ?LOG_NOTICE("Vm started",[Node,DeploymentId]),
	    true;
	false ->
	    ?LOG_NOTICE("Failed to start vm  %%%%%%%%%%%%%%%%%%%%% ",[Node,DeploymentId]),
	    false
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_vm_started(DeploymentId)->
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    case net_adm:ping(Node) of
	pang->
	    false;
	pong->
	    true
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_vm(DeploymentId)->
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    rpc:call(Node,init,stop,[],5000),
    check_stopped_node(Node).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_dir(DeploymentId)->
    {ok,Dir}=sd:call(etcd,db_deploy,read,[dir,DeploymentId],5000),   
    {ok,HostSpec}=sd:call(etcd,db_deploy,read,[host_spec,DeploymentId],5000),
    Msg="rm -rf "++Dir,
    TimeOut=5000,
    ssh_server:send_msg(HostSpec,Msg,TimeOut).
    
   


%%----------------------------------------------------------------------------------------
is_deployed(DeploymentId)->
    {ok,Node}=sd:call(etcd,db_deploy,read,[node,DeploymentId],5000),
    {ok,ProviderSpec}=sd:call(etcd,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,App}=sd:call(etcd,db_provider_spec,read,[app,ProviderSpec],5000),
    Result=case net_adm:ping(Node) of
	       pang->
		   false;
	       pong->
		   case rpc:call(Node,App,ping,[],5000) of
		       pong->
			   true;
		       _ ->
			   false
		   end
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
unique_node_name(AppId,Type)->
    Unique=integer_to_list(os:system_time(),36),
    {ok,AppId++"_"++Unique++"_"++Type}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deployment_id(Node)->
    NodeStr=atom_to_list(Node),
    [DeploymentId,_HostName]=string:tokens(NodeStr,"@"),
    {ok,DeploymentId}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
dir(Node)->
    NodeStr=atom_to_list(Node),
    [Dir,_HostName]=string:tokens(NodeStr,"@"),
    {ok,Dir}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
check_stopped_node(Node)->
    check_stopped_node(100,Node,false).

check_stopped_node(_N,_Node,true)->
    true;
check_stopped_node(0,_Node,Boolean) ->
    Boolean;
check_stopped_node(N,Node,_) ->
 
    Boolean=case net_adm:ping(Node) of
		pong->
		    timer:sleep(500),
		    false;
		pang->
		    true
	    end,
    check_stopped_node(N-1,Node,Boolean).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------	 
check_started_node(Node)->
    check_started_node(100,Node,false).

check_started_node(_N,_Node,true)->
  %  io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    true;
check_started_node(0,_Node,Boolean) ->
  %  io:format("Dbg calling node,Node ~p~n",[{node(),Node,?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean;
check_started_node(N,Node,_) ->
    ?LOG_NOTICE("check start node  ",[N,Node]),
 %   io:format("Dbg calling node,Node ~p~n",[{node(),Node,erlang:get_cookie(),?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(100),
		    false;
		pong->
		    true
	    end,
    check_started_node(N-1,Node,Boolean).
