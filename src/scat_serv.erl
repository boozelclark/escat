-module(scat_serv).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-compile(export_all).
%-define(?POOL, spool).

-record(state, {conn_init = false}).


%%====================================================================
%% API
%%====================================================================


start_link() ->
  lager:debug([{app,brik},{module,?MODULE}],"start_link", []),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
  lager:debug([{app,brik},{module,?MODULE}],"init", []),
  {ok, #state{}}.

shutdown() ->
  lager:notice([{app,brik},{module,?MODULE}],"shutdown", []),
  gen_server:call(?MODULE, {command, shutdown}).
command_list() ->
  lager:notice([{app,brik},{module,?MODULE}],"command_list", []),
  gen_server:call(?MODULE, {command, command_list}).
help() ->
  lager:notice([{app,brik},{module,?MODULE}],"help", []),
  gen_server:call(?MODULE, {command, help}).
version() ->
  lager:notice([{app,brik},{module,?MODULE}],"version", []),
  gen_server:call(?MODULE, {command, version}).
uptime() ->
  lager:notice([{app,brik},{module,?MODULE}],"uptime", []),
  gen_server:call(?MODULE, {command, uptime}).
running_mode() ->
  lager:notice([{app,brik},{module,?MODULE}],"running_mode", []),
  gen_server:call(?MODULE, {command, running_mode}).
capture_mode() ->
  lager:notice([{app,brik},{module,?MODULE}],"capture_mode", []),
  gen_server:call(?MODULE, {command, capture_mode}).
conf_get(Cnf) ->
  lager:notice([{app,brik},{module,?MODULE}],"conf_get", []),
  gen_server:call(?MODULE, {command, conf_get, Cnf}).
dump_counters() ->
  lager:notice([{app,brik},{module,?MODULE}],"dump_counters", []),
  gen_server:call(?MODULE, {command, dump_counters}).
reload_rules() ->
  lager:notice([{app,brik},{module,?MODULE}],"reload_rules", []),
  gen_server:call(?MODULE, {command, reload_rules}).
ruleset_reload_rules() ->
  lager:notice([{app,brik},{module,?MODULE}],"ruleset_reload_rules", []),
  gen_server:call(?MODULE, {command, ruleset_reload_rules}).
ruleset_reload_nonblocking() ->
  lager:notice([{app,brik},{module,?MODULE}],"ruleset_reload_nonblocking", []),
  gen_server:call(?MODULE, {command, ruleset_reload_nonblocking}).
ruleset_reload_time() ->
  lager:notice([{app,brik},{module,?MODULE}],"ruleset_reload_time", []),
  gen_server:call(?MODULE, {command, ruleset_reload_time}).
ruleset_stats() ->
  lager:notice([{app,brik},{module,?MODULE}],"ruleset_stats", []),
  gen_server:call(?MODULE, {command, ruleset_stats}).
ruleset_failed_rules() ->
  lager:notice([{app,brik},{module,?MODULE}],"ruleset_failed_rules", []),
  gen_server:call(?MODULE, {command, ruleset_failed_rules}).
register_tenant_handler(Id, VLAN) ->
  lager:notice([{app,brik},{module,?MODULE}],"register_tenant_handler", []),
  gen_server:call(?MODULE, {command, register_tenant_handler, Id, VLAN}).
unregister_tenant_handler(Id) ->
  lager:notice([{app,brik},{module,?MODULE}],"unregister_tenant_handler", []),
  gen_server:call(?MODULE, {command, unregister_tenant_handler, Id}).
register_tenant(Id, Yaml) ->
  lager:notice([{app,brik},{module,?MODULE}],"register_tenant", []),
  gen_server:call(?MODULE, {command, register_tenant, Id, Yaml}).
reload_tenant(Id) ->
  lager:notice([{app,brik},{module,?MODULE}],"reload_tenant", []),
  gen_server:call(?MODULE, {command, reload_tenant, Id}).
unregister_tenant(Id)->
  lager:notice([{app,brik},{module,?MODULE}],"unregister_tenant", []),
  gen_server:call(?MODULE, {command, unregister_tenant, Id}).
add_hostbit(IP, BitName, ExpireSeconds) ->
  lager:notice([{app,brik},{module,?MODULE}],"add_hostbit", []),
  gen_server:call(?MODULE, {command, add_hostbit, IP, BitName, ExpireSeconds}).
remove_hostbit(IP, BitName) ->
  lager:notice([{app,brik},{module,?MODULE}],"remove_hostbit", []),
  gen_server:call(?MODULE, {command, remove_hostbit, IP, BitName}).
list_hostbit(IP) ->
  lager:notice([{app,brik},{module,?MODULE}],"list_hostbit", []),
  gen_server:call(?MODULE, {command, list_hostbit, IP}).
iface_list() ->
  lager:notice([{app,brik},{module,?MODULE}],"iface_list", []),
  gen_server:call(?MODULE, {command, iface_list}).
iface_stat(IFace) ->
  lager:notice([{app,brik},{module,?MODULE}],"iface_stat", []),
  gen_server:call(?MODULE, {command, iface_stat, IFace}).  
  
  
handle_call({command, shutdown}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command shutdown", []),
 {ok, Result} = send("{\"command\": \"shutdown\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command shutdown ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, command_list}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command command_list", []),
 {ok, Result} = send("{\"command\": \"command-list\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command command_list ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};
 
handle_call({command, help}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command help", []),
 Cmd = "{\"command\": \"help\"}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command help ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, version}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command version", []),
 Cmd = "{\"command\": \"version\"}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command version ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, uptime}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command uptime", []),
 Cmd = "{\"command\": \"uptime\"}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command uptime ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};
 
handle_call({command, running_mode}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command running_mode", []),
 Cmd = "{\"command\": \"running-mode\"}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command running_mode ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};
 
handle_call({command, capture_mode}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command capture_mode", []),
 Cmd = "{\"command\": \"capture-mode\"}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command capture_mode ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, conf_get, Cnf}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command conf_get", []),
 Cmd = "{\"command\": \"conf-get\", \"arguments\": {\"variable\": \"" ++ Cnf ++ "\"}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command conf_get ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};
 %conf_get
 
handle_call({command, dump_counters}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command dump_counters", []),
 {ok, Result} = send("{\"command\": \"dump-counters\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command dump_counters ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, reload_rules}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command reload_rules", []),
 {ok, Result} = send("{\"command\": \"reload-rules\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command reload_rules ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, ruleset_reload_rules}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_rules", []),
 {ok, Result} = send("{\"command\": \"ruleset-reload-rules\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_rules ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, ruleset_reload_nonblocking}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_nonblocking", []),
 {ok, Result} = send("{\"command\": \"ruleset-reload-nonblocking\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_nonblocking ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, ruleset_reload_time}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_time", []),
 {ok, Result} = send("{\"command\": \"ruleset-reload-time\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_reload_time ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, ruleset_stats}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_stats", []),
 {ok, Result} = send("{\"command\": \"ruleset-stats\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_stats ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, ruleset_failed_rules}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_failed_rules", []),
 {ok, Result} = send("{\"command\": \"ruleset-failed-rules\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command ruleset_failed_rules ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, register_tenant_handler, Id, VLAN}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command register_tenant_handler", []),
 Cmd = "{\"command\": \"register-tenant-handler\", \"arguments\": {\"id\": " ++ integer_to_list(Id) ++ ", \"vlan\":" ++ integer_to_list(VLAN) ++ "}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command register_tenant_handler ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, unregister_tenant_handler, Id}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command unregister_tenant_handler", []),
 Cmd = "{\"command\": \"unregister-tenant-handler\", \"arguments\": {\"id\": " ++ integer_to_list(Id) ++ "}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command unregister_tenant_handler ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, register_tenant, Id, Yaml}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command register_tenant", []),
 Cmd = "{\"command\": \"register-tenant\", \"arguments\": {\"id\": " ++ integer_to_list(Id) ++  ", \"yaml\":\"" ++ Yaml ++ "\"}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command register_tenant ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, reload_tenant, Id, Yaml}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command reload_tenant", []),
 Cmd = "{\"command\": \"reload-tenant\", \"arguments\": {\"id\": " ++ integer_to_list(Id) ++  "}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command reload_tenant ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};
 
handle_call({command, unregister_tenant, Id}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command unregister_tenant", []),
 Cmd = "{\"command\": \"unregister-tenant\", \"arguments\": {\"id\": " ++ integer_to_list(Id) ++ "}}",
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command unregister_tenant ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, add_hostbit, IP, BitName, ExpireSeconds}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command add_hostbit", []),
 Cmd = "{\"command\": \"add-hostbit\", \"arguments\": {\"ipaddress\": \"" ++ IP ++  "\", \"hostbit\":\"" ++ BitName ++ "\", \"expire in seconds\":" ++ integer_to_list(ExpireSeconds) ++ "}}",
 lager:notice("~p",[Cmd]),
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command add_hostbit ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, remove_hostbit, IP, BitName}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command remove_hostbit", []),
 Cmd = "{\"command\": \"remove-hostbit\", \"arguments\": {\"ipaddress\": \"" ++ IP ++  "\", \"hostbit\":\"" ++ BitName ++ "\"}}",
 lager:notice("~p",[Cmd]),
 {ok, Result} = send(Cmd, CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command remove_hostbit ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, list_hostbit, IPAddress}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command list_hostbit", []),
 {ok, Result} = send("{\"command\": \"list-hostbit\", \"arguments\": {\"ipaddress\": \"" ++ IPAddress ++  "\"}}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command list_hostbit ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, iface_list}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command iface_list", []),
 {ok, Result} = send("{\"command\": \"iface-list\"}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command get_iface_list ~p", [Result]),
 {reply, Result, State#state{conn_init = true}};

handle_call({command, iface_stat, Interface}, _From, State=#state{conn_init=CInit}) ->
 lager:notice([{app,brik},{module,?MODULE}],"command iface_stat", []),
 {ok, Result} = send("{\"command\": \"iface-stat\", \"arguments\": {\"iface\": \"" ++ atom_to_list(Interface) ++  "\"}}", CInit),
 lager:notice([{app,brik},{module,?MODULE}],"command iface-stat ~p", [Result]),
 {reply, Result, State#state{conn_init = true}}; 

handle_call(Request, From, State) ->
  lager:warning([{app,brik},{module,?MODULE}],"Unknown Call ~p from ~p", [Request, From]),
  {reply, {error, <<"Unknown Call">>}, State}.

handle_cast(Msg, State) ->
  lager:warning([{app,brik},{module,?MODULE}],"Unknown Cast ~p", [Msg]),
  {noreply, State}.

handle_info({udp, Client, _Ip, _Port, Msg}, LoopData) ->
  io:format("receive udp data ~p from ~p~n", [Msg, Client]),
  {noreply, LoopData};

handle_info(Info, State) ->
  lager:warning([{app,brik},{module,?MODULE}],"Unknown Info ~p", [Info]),
  {noreply, State}.

terminate(_Reason, State=#state{}) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send(Msg) ->
  gen_server:call(?MODULE, {message, Msg}).

%%====================================================================
%% Internal functions
%%====================================================================
send(Message, false) ->
  lager:notice([{app,brik},{module,?MODULE}],"send()", []),
  {ok, Version} = tcp_client:sync_send(spool,"{\"version\": \"0.1\"}",3000),
  lager:notice([{app,brik},{module,?MODULE}],"Version: ~p", [Version]),
  send(Message, true);
send(Message, true) ->
  tcp_client:sync_send(spool,Message,3000).
