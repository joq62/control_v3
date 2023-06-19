%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(vm_appl_controller).
 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(ConfigFile_ToBeChanged,"config/sys.config").

-define(DBETCD,dbetcd_appl).


%% External exports
-export([
	 create_deployment/3,
	 create_deployment/4,
	 delete_deployment/1,

	 start_vm/1,
	 stop_vm/1,

%	 create_dir/1,
%	 load_appl/1,
%	 start_appl/1,
%	 stop_appl/1,
%	 unload_appl/1,
%	 delete_dir/1
	 
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
create_deployment(ProviderSpec,HostSpec,Type)->
    {ok,AppName}=db_provider_spec:read(app_name,ProviderSpec),
    {ok,Unique}=unique_node_name(AppName,Type),
    create_deployment(ProviderSpec,HostSpec,Type,Unique).

create_deployment(ProviderSpec,HostSpec,_Type,Unique)->
    DeploymentId=Unique,
    NodeName=Unique,
    Dir=Unique,
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    Node=list_to_atom(Unique++"@"++HostName),
    CreationTime={date(),time()},
    {atomic,ok}=db_deploy:create(DeploymentId,ProviderSpec,NodeName,Dir,Node,HostSpec,CreationTime),
    {ok,DeploymentId}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_deployment(DeploymentId)->
    {atomic,ok}=db_deploy:delete(DeploymentId),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_vm(DeploymentId)->
 
    %% Ensure Node  stopped
    {ok,Node}=db_deploy:read(node,DeploymentId),
    rpc:call(Node,init,stop,[],5000),
    true=check_stopped_node(Node),
    %% ssh start vm    
    CookieStr=atom_to_list(erlang:get_cookie()),
    {ok,NodeName}=db_deploy:read(nodename,DeploymentId),
    {ok,HostSpec}=db_deploy:read(host_spec,DeploymentId),
    LinuxCmd=" -sname "++NodeName++" "++" -setcookie "++CookieStr++" "++" -detached ",
    TimeOut=2*5000,
    ssh_server:send_msg(HostSpec,LinuxCmd,TimeOut),
    check_started_node(Node).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_vm(DeploymentId)->
    {ok,Node}=db_deploy:read(node,DeploymentId),
    rpc:call(Node,init,stop,[],5000),
    check_stopped_node(Node).





%%----------------------------------------------------------------------------------------
is_deployed(DeploymentId)->
    {ok,Node}=sd:call(dbetcd_appl,db_deploy,read,[node,DeploymentId],5000),
    {ok,ProviderSpec}=sd:call(dbetcd_appl,db_deploy,read,[provider_spec,DeploymentId],5000),
    {ok,App}=sd:call(dbetcd_appl,db_provider_spec,read,[app,ProviderSpec],5000),
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
    io:format("Dbg calling node,Node ~p~n",[{node(),Node,erlang:get_cookie(),?MODULE,?FUNCTION_NAME,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(100),
		    false;
		pong->
		    true
	    end,
    check_started_node(N-1,Node,Boolean).
