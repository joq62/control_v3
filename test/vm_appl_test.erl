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
-module(vm_appl_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(NodeNames,["c1","c2","c3"]).
-define(UniqueStr,"unique1").

-define(C50,{"172.26.158.249",22,"joq62","festum01"}).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=create_deployment(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
  
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_deployment()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ProviderSpec="adder",
    HostSpec="c50",
    Type="na",
    UniqueStr=?UniqueStr,
    {ok,DeploymentId}=vm_appl_controller:create_deployment(ProviderSpec,HostSpec,Type,UniqueStr),
    {ok,ProviderSpec}=db_deploy:read(provider_spec,DeploymentId),
    {ok,?UniqueStr}=db_deploy:read(node_name,DeploymentId),
    {ok,'unique1@c50'}=db_deploy:read(node,DeploymentId),
    {ok,UniqueStr}=db_deploy:read(dir,DeploymentId),
    {ok,HostSpec}=db_deploy:read(host_spec,DeploymentId),
    
    {
     "adder","adder","0.1.0","adder",adder,"adder","adder",
     "a_cookie","https://github.com/joq62/adder.git",
     " -pa adder/ebin -config adder/config/sys.config",
     "tar -xvf adder/adder-0.1.0.tar.gz -C adder ",
     {application,start,[adder],20000},1,[all_hosts]
    }=db_provider_spec:read(ProviderSpec),
    {
     "c50","c50","172.26.158.249",22,"joq62","festum01",
     [],"host_controller",'host_controller@c50'
    }=db_host_spec:read(HostSpec),
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {Ip,Port,Uid,Pwd}=?C50,
    TimeOut=5000,
    GitPath="https://github.com/joq62/adder.git",
    Dir="adder",
    NodeName=Dir,
    Cookie="a_cookie",
    App=adder,
    Node=list_to_atom(NodeName++"@"++"c50"),

    {ok,["/home/ubuntu"]}=ssh_server:send_msg(Ip,Port,Uid,Pwd,"pwd",TimeOut),
    %%
    rpc:call(Node,init,stop,[],5000),
    timer:sleep(5000),
    RmDirResult=ssh_server:send_msg(Ip,Port,Uid,Pwd,"rm -rf "++Dir,TimeOut),
    io:format("rm dir ~p~n",[{RmDirResult,?MODULE,?FUNCTION_NAME,?LINE}]),

     MkDirResult=ssh_server:send_msg(Ip,Port,Uid,Pwd,"mkdir "++Dir,TimeOut),
    io:format("mkdir dir ~p~n",[{MkDirResult,?MODULE,?FUNCTION_NAME,?LINE}]),

    GitCloneResult=ssh_server:send_msg(Ip,Port,Uid,Pwd,"git clone "++GitPath++" " ++Dir,TimeOut),
    io:format("GitCloneResult ~p~n",[{GitCloneResult,?MODULE,?FUNCTION_NAME,?LINE}]),

    Ebin=filename:join(Dir,"ebin"),
    Pa=" -pa "++Ebin,
    SysConfig=" -config "++filename:join([Dir,"config","sys.config"]),
    SetCookie=" -setcookie "++Cookie,
    Sname=" -sname "++NodeName,
    Detached=" -detached ",
 %   VmStart=ssh_server:send_msg(Ip,Port,Uid,Pwd,"erl "++Pa++" "++Sname++" "++SetCookie++" "++SysConfig++" "++Sname++" "++Detached,TimeOut),
 %   VmStart=ssh_server:send_msg(Ip,Port,Uid,Pwd,"erl "++Pa++" "++Sname++" "++SetCookie++" "++SysConfig++" "++Sname++" "++Detached,TimeOut),
%    VmStart=ssh_server:send_msg(Ip,Port,Uid,Pwd,"erl -pa adder/ebin -sname adder -setcookie a_cookie -config adder/config/sys.config -detached",TimeOut),
    VmStart=ssh_server:send_msg(Ip,Port,Uid,Pwd,"erl -pa adder/ebin -sname adder -setcookie a_cookie  -detached",TimeOut),
    io:format("VmStart ~p~n",[{VmStart,?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(5000),
    pong=net_adm:ping(Node),
    

    
    
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok.
