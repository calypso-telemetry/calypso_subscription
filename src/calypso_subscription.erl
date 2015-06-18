-module(calypso_subscription).
-author("begemot").

-behaviour(gen_event).

%% API
-export([
  start/3, stop/2,
  add_handler/3, remove_handler/1,
  notify/2
]).

% -callback subs_event(Pid :: pid(), Msg :: term()) -> none.

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  key_fun :: fun((_) -> tuple())
}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start(Name, HookNames, KeyFun) when is_list(HookNames) ->
  lists:foreach(fun(HookName) ->
    calypso_subs_event:add_handler(HookName, ?MODULE, Name, [ Name, HookName, KeyFun ])
  end, HookNames).

stop(Name, HookNames) when is_list(HookNames) ->
  lists:foreach(fun(HookName) ->
    calypso_subs_event:delete_handler(HookName, ?MODULE, Name, [])
  end, HookNames).

notify(HookName, Msg) ->
  calypso_subs_event:notify(HookName, Msg).

add_handler(Key, Module, Data) ->
  calypso_registrar:register(Module, { subs, Key }, Data).

remove_handler(Key) ->
  calypso_registrar:unregister({ subs, Key }).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([ KeyFun ]) ->
  {ok, #state{
    key_fun = KeyFun
  }}.

-spec(handle_event(Event :: term(), State :: #state{}) ->
  {ok, NewState :: #state{}} |
  {ok, NewState :: #state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).
handle_event(Msg, State) ->
  case (State#state.key_fun)(Msg) of
    undefined -> ok;
    none -> ok;
    Key ->
      calypso_registrar:apply_by_id({subs, Key}, subs_event, [ Msg ])
  end,
  {ok, State}.

-spec(handle_call(Request :: term(), State :: #state{}) ->
  {ok, Reply :: term(), NewState :: #state{}} |
  {ok, Reply :: term(), NewState :: #state{}, hibernate} |
  {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  {remove_handler, Reply :: term()}).
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

-spec(handle_info(Info :: term(), State :: #state{}) ->
  {ok, NewState :: #state{}} |
  {ok, NewState :: #state{}, hibernate} |
  {swap_handler, Args1 :: term(), NewState :: #state{},
    Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
  remove_handler).
handle_info(_Info, State) ->
  {ok, State}.

-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
remove_handler | {error, {'EXIT', Reason :: term()}} |
{error, term()}), State :: term()) -> term()).
terminate(_Arg, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
