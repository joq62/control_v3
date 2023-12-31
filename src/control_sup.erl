%%%-------------------------------------------------------------------
%% @doc provider top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(control_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
		  #{id=>lib_ssh,
		    start=>{ssh_server,start_link,[]}},
		  #{id=>control,
		    start=>{control,start_link,[]}},
		  #{id=>control_node,
		   start=>{control_node,start_link,[]}},
		  #{id=>control_provider,
		   start=>{control_provider,start_link,[]}},
		  #{id=>control_orchestrate,
		    start=>{control_orchestrate,start_link,[]}}
	%	  #{id=>control_monitor,
	%	    start=>{control_monitor,start_link,[]}}
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
