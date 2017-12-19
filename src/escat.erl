%%%-------------------------------------------------------------------
%% @doc escat public API
%% @end
%%%-------------------------------------------------------------------

-module(escat).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-compile(export_all).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    escat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


shutdown() ->
  scat_serv:shutdown().
command_list() ->
  scat_serv:command_list().
help() ->
  scat_serv:help().
version() ->
  scat_serv:version().
uptime() ->
  scat_serv:uptime().
running_mode() ->
  scat_serv:running_mode().
capture_mode() ->
  scat_serv:capture_mode().
conf_get(Cnf) ->
  scat_serv:conf_get(Cnf).
dump_counters() ->
  scat_serv:dump_counters().
reload_rules() ->
  scat_serv:reload_rules().
ruleset_reload_rules() ->
  scat_serv:ruleset_reload_rules().
ruleset_reload_nonblocking() ->
  scat_serv:ruleset_reload_nonblocking().
ruleset_reload_time() ->
  scat_serv:ruleset_reload_time().
ruleset_stats() ->
  scat_serv:ruleset_stats().
ruleset_failed_rules() ->
  scat_serv:ruleset_failed_rules().
register_tenant_handler(Id, VLAN) ->
  scat_serv:register_tenant_handler(Id, VLAN).
unregister_tenant_handler(Id) ->
  scat_serv:unregister_tenant_handler(Id).
register_tenant(Id, Yaml) ->
  scat_serv:register_tenant(Id, Yaml).
reload_tenant(Id) ->
  scat_serv:reload_tenant(Id).
unregister_tenant(Id)->
  scat_serv:unregister_tenant(Id).
add_hostbit(IP, BitName, ExpireSeconds) ->
  scat_serv:add_hostbit(IP, BitName, ExpireSeconds).
remove_hostbit(IP, BitName) ->
  scat_serv:remove_hostbit(IP, BitName).
list_hostbit(IP) ->
  scat_serv:list_hostbit(IP).
iface_list() ->
  scat_serv:iface_list().
iface_stat(IFace) ->
  scat_serv:iface_stat(IFace).


%%====================================================================
%% Internal functions
%%====================================================================
