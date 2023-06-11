
-define(TABLE,deploy).
-define(RECORD,deploy).

-record(?RECORD,{
		 id,
		 provider_spec,
		 node_name,
		 dir,
		 node,
		 host_spec,
		 creation_time
		}).
