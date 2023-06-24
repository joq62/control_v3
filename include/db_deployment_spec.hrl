-define(SpecDir,"deployments").
-define(GitPathSpecs,"https://github.com/joq62/deployments.git").

-define(TABLE,deployment_spec).
-define(RECORD,deployment_spec).

-record(?RECORD,{
		 spec,
		 deployment
		}).
