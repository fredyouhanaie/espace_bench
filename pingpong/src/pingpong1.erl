%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fy@localhost>
%%% @copyright (C) 2020, Fred Youhanaie
%%% @doc
%%%
%%% Tuplespace ping-pong test. This is a basic performance test based
%%% on the `pingpong' program in:
%%%
%%% N.Carriero, D.Gelernter. "The S/Net's Linda Kernel", ACM
%%% Transaction on Computer Systems, Vol. 4, No. 2, pp110-129, May
%%% 1986.
%%%
%%% There are two workers, `ping' and `pong'.
%%%
%%% `ping' will repeatedly wait/block for a `{ping}' tuple, once
%%% received, it will output a `{pong}' tuple. `ping' will stop after
%%% `LIMIT' pings, and it will output a `{done}' tuple. The `{done}'
%%% tuple is for the benefit of the main function that start the whole
%%% benchmark.
%%%
%%% `pong' will repeatedly output a `{ping}' tuple and wait/block for
%%% a `{pong}' tuple.
%%%
%%% @end Created : 5 Dec 2020 by Fred Youhanaie <fy@localhost>
%%%-------------------------------------------------------------------
-module(pingpong1).

%%--------------------------------------------------------------------

-export([start/0, start/1]).
-export([ping/1, pong/0]).

%%--------------------------------------------------------------------

-define(Default_LIMIT, 20000).

%%--------------------------------------------------------------------

-spec ping(integer()) -> done.
ping(0) ->
    done;

ping(Count) ->
    espace:in({ping}),
    espace:out({pong}),
    ping(Count-1).

%%--------------------------------------------------------------------

-spec pong() -> none.
pong() ->
    espace:out({ping}),
    espace:in({pong}),
    pong().

%%--------------------------------------------------------------------

-spec start() -> ok.
start() ->
    start(?Default_LIMIT).

%%--------------------------------------------------------------------

-spec start(integer()) -> ok.
start(LIMIT) ->
    logger:set_primary_config(level, error),

    espace:start(),

    {Time_usec, _Result} = timer:tc(fun doit/1, [LIMIT]),

    espace:stop(),
    timer:sleep(100), %% flush any outstanding log messages

    Time_sec = Time_usec / 1_000_000,
    io:format("Pairs=~p, Time=~p sec, Pairs/sec=~p.~n",
              [LIMIT, Time_sec, LIMIT/Time_sec]).

%%--------------------------------------------------------------------

doit(LIMIT) ->
    espace:eval({{fun ping/1, [LIMIT]}}),
    espace:worker({fun pong/0, []}),
    espace:in({done}).

%%--------------------------------------------------------------------
