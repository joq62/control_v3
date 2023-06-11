
-define(TABLE,lock).
-define(RECORD,lock).

-record(?RECORD,{
		 lock_id,
		 transaction_id,
		 time,
		 status
		}).
