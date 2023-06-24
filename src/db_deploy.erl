%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>
-module(db_deploy).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_deploy.hrl").
%% External exports

-export([create_table/0,create_table/2,add_node/2]).
-export([create/8,delete/1]).
-export([read_all/0,read/1,read/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).

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

create(Id,DeploymentSpec,ProviderSpec,NodeName,Dir,Node,HostSpec,CreationTime)->
    Record=#?RECORD{
		    id=Id,
		    deployment_spec=DeploymentSpec,
		    provider_spec=ProviderSpec,
		    node_name=NodeName,
		    node=Node,
		    dir=Dir,
		    host_spec=HostSpec,
		    creation_time=CreationTime
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

member(Id)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.id==Id])),
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
    [{R#?RECORD.id,R#?RECORD.deployment_spec,R#?RECORD.provider_spec,R#?RECORD.node_name,
      R#?RECORD.dir,R#?RECORD.node, R#?RECORD.host_spec,R#?RECORD.creation_time}||R<-Z].

read(Id)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.id==Id])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.id,R#?RECORD.deployment_spec,R#?RECORD.provider_spec,R#?RECORD.node_name,
			    R#?RECORD.dir,R#?RECORD.node, R#?RECORD.host_spec,R#?RECORD.creation_time}||R<-Z],
		   Info
	   end,
    Result.

read(Key,Id)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.id==Id])),
    Result=case Z of
	       []->
		   {error,[eexist,Id,?MODULE,?LINE]};
	       [R] ->
		   case  Key of
		       id->
			   {ok,R#?RECORD.id};
		       deployment_spec->
			   {ok,R#?RECORD.deployment_spec};
		       provider_spec->
			   {ok,R#?RECORD.provider_spec};
		       node_name->
			   {ok,R#?RECORD.node_name};
		       dir->
			   {ok,R#?RECORD.dir};
		       node->
			   {ok,R#?RECORD.node};
		       host_spec->
			   {ok,R#?RECORD.host_spec};
		       creation_time->
			   {ok,R#?RECORD.creation_time};
		       Err ->
			   {error,['Key eexists',Err,Id,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.id||R<-Z].
    

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
