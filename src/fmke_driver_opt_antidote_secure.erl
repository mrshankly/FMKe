%% ---------------------------------------------------------------------------------------------------------------------
%% Database driver for AntidoteDB, featuring a normalized data model (no CRDT nesting) and secure CRDTs.
%% ---------------------------------------------------------------------------------------------------------------------
-module(fmke_driver_opt_antidote_secure).

% -behaviour(fmke_gen_driver).
-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server exports
-export ([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-define(ANTIDOTE_TRANSACTION_RETRIES, 3).
-define(MAP, antidote_crdt_map_go).
-define(LWWREG, antidote_crdt_register_lww).
-define(ORSET, antidote_crdt_set_aw).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, _Started} = application:ensure_all_started(antidotec_pb),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({_Op, _Client}, State) ->
    {noreply, State}.