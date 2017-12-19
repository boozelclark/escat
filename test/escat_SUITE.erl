-module(escat_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test1/1, 
    command_list_test/1,
    help_test/1,
    version_test/1,
    uptime_test/1,
    running_mode_test/1,
    capture_mode_test/1,
    conf_get_test/1,
    dump_counters_test/1,
    reload_rules_test/1,
    ruleset_reload_rules_test/1,
    ruleset_reload_nonblocking_test/1,
    ruleset_reload_time_test/1,
    ruleset_stats_test/1,
    ruleset_failed_rules_test/1,
    register_tenant_handler_test/1,
    shutdown_test/1
]).
 
all() -> [
    command_list_test,
    help_test,
    version_test,
    uptime_test,
    running_mode_test,
    capture_mode_test,
    conf_get_test,
    dump_counters_test,
    reload_rules_test,
    ruleset_reload_rules_test,
    ruleset_reload_nonblocking_test,
    ruleset_reload_time_test,
    ruleset_stats_test,
    ruleset_failed_rules_test,
    register_tenant_handler_test,
    unregister_tenant_handler_test,
    register_tenant_test,
    reload_tenant_test,
    unregister_tenant_test,
    add_hostbit_test,
    remove_hostbit_test,
    list_hostbit_test,
    iface_list_test,
    iface_stat_test,
    shutdown_test
].
 
init_per_suite(Config) ->
    %application:set_env(escat, pools, [{spool, 1,[{address, {local, "/run/suricata/suricata-command.socket"}},{port, 0},{options, []}]}]),
    {ok, _} = application:ensure_all_started(tcp_client),
    lager:info([{app,escat},{module,?MODULE}],"info", []),
    [].

%tests that hit localhost:port..

end_per_suite(Config) ->
    ok = application:stop(escat).
 

test1(_Config) ->
  eunit:test(escat_tests),
  true.
  
%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

shutdown_test(_Config) ->
  R = escat:shutdown(),
  process_result(R).
command_list_test(_Config) ->
  R = escat:command_list(),
  process_result(R).
help_test(_Config) ->
  R = escat:help(),
  process_result(R).
version_test(_Config) ->
  R = escat:version(),
  process_result(R).
uptime_test(_Config) ->
  R = escat:uptime(),
  process_result(R).
running_mode_test(_Config) ->
  R = escat:running_mode(),
  process_result(R).
capture_mode_test(_Config) ->
  R = escat:capture_mode(),
  process_result(R).
conf_get_test(_Config) ->
  R = escat:conf_get("unix-command.enabled"),
  process_result(R).
dump_counters_test(_Config) ->
  R = escat:dump_counters(),
  process_result(R).
reload_rules_test(_Config) ->
  R = escat:reload_rules(),
  process_result(R).
ruleset_reload_rules_test(_Config) ->
  R = escat:ruleset_reload_rules(),
  process_result(R).
ruleset_reload_nonblocking_test(_Config) ->
  R = escat:ruleset_reload_nonblocking(),
  process_result(R).
ruleset_reload_time_test(_Config) ->
  R = escat:ruleset_reload_time(),
  process_result(R).
ruleset_stats_test(_Config) ->
  R = escat:ruleset_stats(),
  process_result(R).
ruleset_failed_rules_test(_Config) ->
  R = escat:ruleset_failed_rules(),
  process_result(R).
register_tenant_handler_test(_Config) ->
  R = escat:register_tenant_handler_test(),
  process_result(R).
unregister_tenant_handler_test(_Config) ->
  R = escat:unregister_tenant_handler(1).
register_tenant_test(_Config) ->
  R = escat:register_tenant(1, "1.yaml").
reload_tenant_test(_Config) ->
  R = escat:reload_tenant(1).
unregister_tenant_test()->
  R = escat:unregister_tenant_test().
add_hostbit_test(_Config) ->
  R = escat:add_hostbit("192.168.1.95", "blacklist", 3600).
remove_hostbit_test(_Config) ->
  R = escat:remove_hostbit("192.168.1.95", "blacklist").
list_hostbit_test(_Config) ->
  R = escat:list_hostbit("192.168.1.95").
iface_list_test(_Config) ->
  R = escat:iface_list().
iface_stat_test(_Config) ->
  R = escat:iface_stat(wlan0).

process_result(Result) ->
  RS1 = jsx:decode(Result, [return_maps]),
  #{<<"return">>:=<<"OK">>} = RS1,
  true.

  

