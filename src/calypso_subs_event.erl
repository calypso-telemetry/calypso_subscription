-module(calypso_subs_event).
-author("begemot").

%% API
-export([
  start_link/1,
  add_handler/4, add_sup_handler/4, delete_handler/4,
  notify/2
]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link(Name) ->
  { ok, Pid } = gen_event:start_link(),
  gen_event:add_handler(Pid, calypso_subs_event, [ Name ]),
  { ok, Pid }.

add_sup_handler(Name, Module, Id, Args) ->
  Pid = pid_or_start(Name),
  gen_event:add_sup_handler(Pid, { Module, Id }, Args).

add_handler(Name, Module, Id, Args) ->
  Pid = pid_or_start(Name),
  gen_event:add_handler(Pid, { Module, Id }, Args).

delete_handler(Name, Module, Id, Args) ->
  case pid(Name) of
    undefined -> ok;
    Pid ->
      gen_event:delete_handler(Pid, { Module, Id }, Args)
  end.

notify(Name, Msg) ->
  raw_notify(pid(Name), Msg).

raw_notify(undefined, _) -> ok;
raw_notify(Pid, Msg) when is_pid(Pid) ->
  gen_event:notify(Pid, Msg).

hook_fire(Msg) ->
  case calypso_hooks:get_hook_name() of
    undefined -> ok;
    Name ->
      calypso_subs_event:notify(Name, { hook, Name, Msg })
  end,
  none.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

-spec(init(InitArgs :: term()) ->
  {ok, State :: #state{}} |
  {ok, State :: #state{}, hibernate} |
  {error, Reason :: term()}).
init([ Name ]) ->
  calypso_registrar:register(?MODULE, { subs, Name}),
  calypso_hooks:add(Name, ?MODULE, fun ?MODULE:hook_fire/1 ),
  {ok, #state{}}.

handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = error,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pid_or_start(Name) ->
  case pid(Name) of
    undefined ->
      { ok, Pid } = supervisor:start_child(calypso_subscription_sup, { Name, { calypso_subs_event, start_link, [ Name ] }, permanent, 2000, worker, [ calypso_subs_event, calypso_subs_server ]}),
      Pid;
    Pid -> Pid
  end.

pid(Name) ->
  case calypso_registrar:process({subs, Name}) of
    undefined -> undefined;
    Process -> calypso_registrar:pid(Process)
  end.


