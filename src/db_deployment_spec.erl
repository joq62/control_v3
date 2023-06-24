%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_deployment_spec).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_deployment_spec.hrl").

%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([create/1,create/2,delete/1]).
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
create(SpecId,Deployment)->
 Record=#?RECORD{
		 spec=SpecId,
		 deployment=Deployment
		},
    create(Record).

create(Record)->
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(Spec) ->
    F = fun() ->
                mnesia:delete({?TABLE,Spec})

        end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
member(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec==SpecId])),
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

read(Key,Spec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec==Spec])),
    Result=case Z of
	       []->
		   {error,[eexist,Spec,?MODULE,?LINE]};
	       [R]->
		   case  Key of
		       deployment->
			   {ok,R#?RECORD.deployment};
		       Key ->
			   {error,['Key eexists',Key,Spec,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.spec||R<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.spec,R#?RECORD.deployment
     }||R<-Z].

read(Spec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec==Spec])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.spec,R#?RECORD.deployment
			   }||R<-Z],
		   Info
	   end,
    Result.

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
			   file:del_dir_r(TempDirName),    
	%		   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   file:del_dir_r(TempDirName),    
			 %  os:cmd("rm -rf "++TempDirName),	
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
    GitDir=filename:join(TempDirName,?SpecDir),
    GitPath=?GitPathSpecs,
    file:del_dir_r(GitDir),    
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

from_file(SpecDir)->
    {ok,FileNames}=file:list_dir(SpecDir),
 %   io:format("FileNames  ~p~n",[{FileNames,?MODULE,?FUNCTION_NAME,?LINE}]),
    from_file(FileNames,SpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    FullFileName=filename:join(Dir,FileName),
  %  io:format("FullFileName  ~p~n",[{FullFileName,?MODULE,?FUNCTION_NAME,?LINE}]),
    NewAcc=case file:consult(FullFileName) of
	       {error,Reason}->
%		   io:format("error,Reason  ~p~n",[{error,Reason,?MODULE,?FUNCTION_NAME,?LINE}]),
		   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
	       {ok,[{deployment_spec,SpecId,Info}]}->		   
		   Record=#?RECORD{
				   spec=SpecId,
				   deployment=Info
				   },

		   %io:format("Record  ~p~n",[{Record,?MODULE,?LINE}]),
		   case create(Record) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   io:format("NotAnApplSpecFile  ~p~n",[{NotAnApplSpecFile,?MODULE,?FUNCTION_NAME,?LINE}]),
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
			   
