%%%-------------------------------------------------------------------
%% @doc control public API
%% @end
%%%-------------------------------------------------------------------

-module(control_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok=case application:get_env(first_control) of
	undefined ->
	    application:set_env([{control,[{first_control,true}]}]);
	false->
	    ok;
	true->
	    ok
    end,
    {ok,_}=control_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
