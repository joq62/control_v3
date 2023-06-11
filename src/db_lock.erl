-module(db_lock).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_lock.hrl").

-define(LockTimeOut, 2*60). %% 30 sec 


create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create({?MODULE,LockId}) ->
    create(LockId,0).
create(LockId,Time) ->
    F = fun() ->
		Record=#?RECORD{lock_id=LockId,
				transaction_id=na,
				time=Time,
				status=na},		
		mnesia:write(Record) end,
    mnesia:transaction(F).



read_all_info() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.lock_id,R#?RECORD.transaction_id,
      R#?RECORD.time,R#?RECORD.status}||R<-Z].

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.lock_id||R<-Z].

	


read(LockId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.lock_id==LockId])),
    [{R#?RECORD.lock_id,R#?RECORD.transaction_id,
      R#?RECORD.time,R#?RECORD.status}||R<-Z].

try_lock(LockId)->
    try_lock(LockId,?LockTimeOut).
try_lock(LockId,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,LockId}) of
		  []->
		      {mnesia:abort([{error,[eexists,LockId]}])};
		  [LockInfo] ->
		      
		      CurrentTime=erlang:system_time(millisecond),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut-> %% Caller didnt un_lock or it timeout
			      TransactionId=erlang:system_time(microsecond),
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,
							 status=locked,
							 transaction_id=TransactionId},
			      {mnesia:write(LockInfo1),TransactionId};
			  TimeDiff == LockTimeOut->
			      TransactionId=erlang:system_time(microsecond),
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime,
							 status=locked,
							 transaction_id=TransactionId},
			      {mnesia:write(LockInfo1),TransactionId};
			  TimeDiff < LockTimeOut->
			       mnesia:abort(locked)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,{ok,TransactionId}}->
		   {ok,TransactionId};
	       {aborted,Reason}->
		   Reason
	   end,
    Result.

unlock(LockId,TransactionId)->
    F=fun()->
	      case mnesia:read({?TABLE,LockId}) of
		  []->
		      {mnesia:abort({error,[eexists,LockId]})};
		  [LockInfo] ->
		      CurrentTransactionId=LockInfo#?RECORD.transaction_id,
		      case TransactionId==CurrentTransactionId of
			  false->
			      mnesia:abort({error,["eexists Transactions id",TransactionId,CurrentTransactionId]});
			  true->
			      LockInfo1=LockInfo#?RECORD{
							 time=0,
							 status=unlocked,
							 transaction_id=na},
			      mnesia:write(LockInfo1)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,ok}->
		   ok;
	       {aborted,Reason}->
		   Reason
	   end,
    Result.
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
   
is_open(LockId)->
    is_open(LockId,?LockTimeOut).
is_open(LockId,LockTimeOut)->
    F=fun()->
	      case mnesia:read({?TABLE,LockId}) of
		  []->
		      {mnesia:abort({error,[eexists,LockId]})};
		  [LockInfo] ->
		      
		      CurrentTime=erlang:system_time(millisecond),
		      LockTime=LockInfo#?RECORD.time,
		      TimeDiff=CurrentTime-LockTime,
		      if
			  TimeDiff > LockTimeOut->
			      
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff == LockTimeOut->
			      LockInfo1=LockInfo#?RECORD{time=CurrentTime},
			      mnesia:write(LockInfo1);
			  TimeDiff < LockTimeOut->
			       mnesia:abort(LockId)
		      end
	      end
      end,
    Result=case mnesia:transaction(F) of
	       {atomic,ok}->
		   true;
	       {aborted,LockId}->
		   false;
	       {aborted,{error,[eexists,LockId]}}->
		   {error,[eexists,LockId]}
	   end,
    Result.
		      
	      
delete(LockId) ->

    F = fun() -> 
		RecordList=[X||X<-mnesia:read({?TABLE,LockId}),
			    X#?RECORD.lock_id==LockId],
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
