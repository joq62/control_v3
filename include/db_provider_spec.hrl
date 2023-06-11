-define(ProviderSpecDir,"provider_specs").
-define(GitPathProviderSpecs,"https://github.com/joq62/provider_specs.git").

-define(TABLE,provider_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec,
		 appl_name,
		 vsn,
		 app_name,
		 app,
		 dir,
		 node_name,
		 cookie,
		 pa_args,
		 tar_file,
		 git_path,
		 tar_cmd,
		 start_cmd,
		 num,
		 affinity	
		}).


