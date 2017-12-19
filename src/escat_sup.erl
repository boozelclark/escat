%%%-------------------------------------------------------------------
%% @doc escat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(escat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  MaxRestart = 6,
  MaxTime = 3000,
  Children = [
    {scat_serv,
      {scat_serv, start_link, []},
       permanent, 5000, worker, [scat_serv]}
  ],
  {ok, {{one_for_one, MaxRestart, MaxTime},Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
