%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(deployment_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    ok=setup(),
    ok=read_specs_test(),
 
    io:format("End testing  SUCCESS!! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
%    init:stop(),
%    timer:sleep(3000),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
read_specs_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    AllDepSpecs=lists:sort(db_deployment_spec:get_all_id()),
    true=lists:member("test",AllDepSpecs),
    
    {"test",
     [{"dbetcd_appl","c50"},{"dbetcd_appl","c50"},{"adder","c50"},{"adder","c50"},{"adder","c50"},
      {"divi","c50"},
      {"test_appl","c50"},{"test_appl","c50"}
     ]
    }=db_deployment_spec:read("test"),
    
    {ok,
     [
      {"dbetcd_appl","c50"},{"dbetcd_appl","c50"},{"adder","c50"},{"adder","c50"},{"adder","c50"},
      {"divi","c50"},
      {"test_appl","c50"},{"test_appl","c50"}
     ]
    }=db_deployment_spec:read(deployment,"test"),
    
    {error,[eexist,"glurk",db_deployment_spec,_]}=db_deployment_spec:read(deployment,"glurk"),
    {error,['Key eexists',glurk,"test",db_deployment_spec,_]}=db_deployment_spec:read(glurk,"test"),
 
    ok. 
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

   
    ok.
