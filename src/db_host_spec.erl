%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(db_host_spec).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("db_host_spec.hrl").

%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/9,delete/1]).
-export([read_all/0,read/1,read/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create(SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig,ConnectNodeName,ConnectNode)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    hostname=HostName,
		    local_ip=LocalIp,
		    ssh_port=SshPort,
		    uid=Uid,
		    passwd=Passwd,
		    application_config=ApplConfig,
		    connect_node_name=ConnectNodeName,
		    connect_node=ConnectNode
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.spec_id,R#?RECORD.hostname,R#?RECORD.local_ip,R#?RECORD.ssh_port,
      R#?RECORD.uid,R#?RECORD.passwd,R#?RECORD.application_config,
      R#?RECORD.connect_node_name,R#?RECORD.connect_node}||R<-Z].

read(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.spec_id,R#?RECORD.hostname,R#?RECORD.local_ip,
			    R#?RECORD.ssh_port,R#?RECORD.uid,R#?RECORD.passwd,
			    R#?RECORD.application_config,R#?RECORD.connect_node_name,
			    R#?RECORD.connect_node}||R<-Z],
		   Info
	   end,
    Result.

read(Key,SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Result=case Z of
	       []->
		   {error,[eexist,SpecId,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       hostname->
			   {ok,R#?RECORD.hostname};
		       local_ip->
			   {ok,R#?RECORD.local_ip};
		       ssh_port->
			   {ok,R#?RECORD.ssh_port};
		       uid->
			   {ok,R#?RECORD.uid};
		       passwd->
			   {ok,R#?RECORD.passwd};
		       application_config->
			   {ok,R#?RECORD.application_config};
		       connect_node_name->
			   {ok,R#?RECORD.connect_node_name};
		       connect_node->
			   {ok,R#?RECORD.connect_node};
		       Err ->
			   {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.spec_id||R<-Z].
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

git_clone_load()->
    ok=create_table(),
    Result=case git_clone() of
	       {error,Reason}->
		   {error,Reason};
	       {ok,TempDirName,SpecDir}->
		   case from_file(SpecDir) of
		       {error,Reason}->
			   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   os:cmd("rm -rf "++TempDirName),	
			   LoadResult
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    GitDir=filename:join(TempDirName,?HostSpecDir),
    GitPath=?GitPathHostSpecs,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    GitResult=cmn_appl:git_clone_to_dir(node(),GitPath,GitDir),
    Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

from_file(ApplSpecDir)->
    {ok,FileNames}=file:list_dir(ApplSpecDir),
    from_file(FileNames,ApplSpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    FullFileName=filename:join(Dir,FileName),
    NewAcc=case file:consult(FullFileName) of
	       {error,Reason}->
		   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
	       {ok,[{host_spec,SpecId,Info}]}->
		   {hostname,HostName}=lists:keyfind(hostname,1,Info),
		   {local_ip,LocalIp}=lists:keyfind(local_ip,1,Info),
		   {ssh_port,SshPort}=lists:keyfind(ssh_port,1,Info),
		   {uid,Uid}=lists:keyfind(uid,1,Info),
		   {passwd,Passwd}=lists:keyfind(passwd,1,Info),
		   {application_config,ApplConfig}=lists:keyfind(application_config,1,Info),
		   {connect_node_name,ConnectNodeName}=lists:keyfind(connect_node_name,1,Info),
		   {connect_node,ConnectNode}=lists:keyfind(connect_node,1,Info),		 
		 
		   case create(SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig,ConnectNodeName,ConnectNode) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
			   
