%% --------------------------------------------------------------
%% Database driver for AQL, an SQL-like interface for AntidoteDB.
%% --------------------------------------------------------------

-module(fmke_driver_opt_aql).

-behaviour(gen_server).

-include("fmke.hrl").
-include("fmke_kv.hrl").

%% gen_server exports
-export([
    start_link/1,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

init([]) ->
    {ok, _Started} = application:ensure_all_started(aqlc),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({Op, Client}, State) ->
    Reply = call(Op),
    gen_server:reply(Client, Reply),
    poolboy:checkin(handlers, self()),
    {noreply, State}.

% TODOAQL
call(_) ->
    ok.
