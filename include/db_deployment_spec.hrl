-define(SpecDir,"deployments").
-define(GitPathSpecs,"https://github.com/joq62/deployments.git").

-define(TABLE,deployment).
-define(RECORD,deployment).

-record(?RECORD,{
		 spec_id,
		 deployment
		}).
