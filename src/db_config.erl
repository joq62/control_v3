%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_config).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	start/0
	]).
%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    ok=db_provider_spec:create_table(),
    ProviderSpecList=db_provider_spec:git_clone_load(),
    Ok_ProviderSpec=[X||{ok,X}<-ProviderSpecList],
    Err_ProviderSpec=[X||{error,X}<-ProviderSpecList],
 
    ok=db_host_spec:create_table(),
    HostSpecList=db_host_spec:git_clone_load(),
    Ok_HostSpec=[X||{ok,X}<-HostSpecList],
    Err_HostSpec=[X||{error,X}<-HostSpecList],

%    ok=db_deployment_spec:create_table(),
%    DeploySpecList=db_deployment_spec:git_clone_load(),
  %  Ok_DeploySpec=[X||{ok,X}<-DeploySpecList],
  %  Err_DeploySpec=[X||{error,X}<-DeploySpecList],

    ok=db_lock:create_table(),
    ok=db_deploy:create_table(),
    
    Test=lists:append([Ok_ProviderSpec,Ok_HostSpec,
		       Err_ProviderSpec,Err_HostSpec]),
    Result=case Test of
	       []->
		   {error,[
			   %{deployment_spec,Ok_DeploySpec,Err_DeploySpec},
			   {provider_spec, Ok_ProviderSpec,Err_ProviderSpec},
			   {host_spec,Ok_HostSpec,Err_HostSpec}
			  ]};
	       _ ->
		   ok
	   end,
    Result.
