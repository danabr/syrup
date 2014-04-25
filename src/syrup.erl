%% @doc Syrup - a persistent erlang:apply_after.
%% Tasks are stored persistently so that tasks won't be lost
%% by restarting a node.
%% @copyright 2014 Daniel Abrahamsson
%% Licensed under the MIT license.
-module(syrup).

-behaviour(gen_server).

%% API
-export([ delay/4, start/0, start/1, start_link/0, start_link/1, stop/0 ]).

%% Gen server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, { file :: string() }).

-record(task, { key        :: term()
              , run_after  :: non_neg_integer()
              , module     :: atom()
              , function   :: atom()
              , arguments  :: [term()] }).
-define(TABLE, ?MODULE).
-define(SERVER, ?MODULE).

%%%_* API ==============================================================
%% @doc Alias for start([]).
-spec start() -> {ok, pid()} | {error, term()}.
start() -> start([]).

%% @doc Start the syrup server with custom options and without linking to it.
%% see start_link/1 for possible options.
-spec start([{atom(), term()}]) -> {ok, pid()} | {error, term()}.
start(Options) ->
  gen_server:start({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Alias for start_link([]).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> start_link([]).

%% @doc Start the syrup server with custom options.
%% Unknown options will be ignored.
%% Available options:
%% file :: string() - Path to file where tasks will be stored.
%%                    Defaults to "syrup".
-spec start_link([{atom(), term()}]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Stop the server
-spec stop() -> ok.
stop() -> gen_server:call(?SERVER, stop).

%% @doc Delay calling the given MFA with DelayS seconds.
-spec delay(DelayS::non_neg_integer(), M::atom(), F::atom(), [term()]) -> ok.
delay(DelayS, M, F, Args) ->
  gen_server:call(?SERVER, {delay, {DelayS, M, F, Args}}).

%%%_* gen_server callbacks =============================================
init(Options) ->
  State = state_from_options(Options),
  {ok, ?TABLE} = dets:open_file(?TABLE, [ {auto_save, infinity}
                                        , {keypos, #task.key}
                                        , {file, State#state.file}
                                        ]),
  restore_jobs(),
  {ok, State}.

terminate(_Reason, _State) -> dets:close(?TABLE).

code_change(_OldVsn, State, _Extra) -> State.

handle_call(stop, _From, State)                                     ->
  {stop, normal, ok, State};
handle_call({delay, {_DelayS, _M, _F, _A}=EntryData}, _From, State) ->
  Entry = make_job(EntryData),
  persist_job(Entry),
  queue_job(Entry),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  {stop, {unexpected_cast, Msg}, State}.

handle_info({perform, Entry}, State) ->
  perform_job(Entry),
  {noreply, State}.

%%%_* Internal ========================================================
perform_job(#task{module=M, function=F, arguments=A}=Entry) ->
  perform_job(M, F, A),
  delete_job(Entry).

perform_job(M, F, A) ->
  spawn(M, F, A).

make_job({DelayS, M, F, A}) ->
  #task{ key = erlang:make_ref()
       , run_after = current_time() + DelayS
       , module = M
       , function = F
       , arguments = A
       }.

persist_job(Entry) ->
  ok = dets:insert(?TABLE, [Entry]),
  ok = dets:sync(?TABLE).

delete_job(Entry) ->
  ok = dets:delete(?TABLE, Entry#task.key),
  ok = dets:sync(?TABLE).

queue_job(Entry) ->
  Delay = calculate_delay(Entry),
  erlang:send_after(Delay, self(), {perform, Entry}).

restore_jobs() ->
  dets:traverse(?TABLE, fun restore_job/1).

restore_job(Entry) ->
  queue_job(Entry),
  continue.

calculate_delay(#task{run_after=RunTime}) ->
  1000 * erlang:max(0, RunTime - current_time()).

current_time() ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

state_from_options(Options) ->
  state_from_options(Options, default_state()).

state_from_options([], State)                                     -> State;
state_from_options([{file, File}|Rest], State) when is_list(File) ->
  state_from_options(Rest, State#state{file=File});
state_from_options([_|Rest], State)                               ->
  state_from_options(Rest, State).

default_state() -> #state{ file="syrup" }.
