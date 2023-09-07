%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% Collection of functions for processing the baseline dump file.
%%%
%%% @end
%%% Created : 2 Sep 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(r3bench_dump).

-export([print/1, table/1]).

%%--------------------------------------------------------------------

%% below definitions taken from rebar3_bench:rebar_bench_prv.erl
%%
-define(DUMP_FMT_VERSION, [0, 1, 0]).
-define(DUMP_010_FIELDS,  [wall_time, memory, reductions]).

-type param() :: wall_time | memory | reductions.

%%--------------------------------------------------------------------
%% @doc read and print a baseline dump file.
%%
%% This is work in progess. In due course, we will have separate
%% functions to `read', `process' and `output' the data.
%%
%% @end
%%--------------------------------------------------------------------
-spec print(file:name_all()) -> ok.
print(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {Version, Samples} = erlang:binary_to_term(Data),
    io:format("version=~p.~n", [Version]),
    [ io:format("Mod=~p, Fun=~p, Data=~p.~n", [M, F, D])
      || {M, F, D} <- Samples ],
    ok.

%%--------------------------------------------------------------------
%% @doc return a list of list of measurements.
%%
%% Each element of the returned list represents a single raw
%% measurement, [`Mod', `Fun', `Seq', `Param', `Value'].
%%
%% `Seq' is the sequence number within the `{Mod, Fun}' samples. It
%% starts from zero for each set of `{Mod, Fun}' samples. e.g.
%%
%% <pre>
%%   [{bench_espace_1,bench_out_1,0,wall_time,4709.655433486903},
%%    {bench_espace_1,bench_out_1,0,memory,0.0},
%%    {bench_espace_1,bench_out_1,0,reductions,21.000289957795033},
%%    {bench_espace_1,bench_out_1,1,wall_time,6257.570508070492},
%%    {bench_espace_1,bench_out_1,1,memory,0.0},
%%    {bench_espace_1,bench_out_1,1,reductions,21.000289957795033},
%%    {bench_espace_1,bench_out_1,2,wall_time,7043.158639131415},
%%    {bench_espace_1,bench_out_1,2,memory,0.0},
%%    {bench_espace_1,bench_out_1,2,reductions,21.000289957795033},
%%    ...
%% </pre>
%%
%% @end
%%--------------------------------------------------------------------
-spec table(file:name_all()) -> [tuple()].
table(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {Version, Samples} = erlang:binary_to_term(Data),
    samples_to_table(Version, Samples).

%%--------------------------------------------------------------------
%% @doc convert the raw samples structure to a list of lists.
%%
%% Each inner list will contain the following:
%%
%% - module
%% - function
%% - sequence
%% - param
%% - value
%%
%% The `param' will be based on `Version', currently `[0,1,0]'.
%%
%% @end
%%--------------------------------------------------------------------
-spec samples_to_table([integer()], [tuple()]|[]) -> [tuple()]|[].
samples_to_table(?DUMP_FMT_VERSION, Samples) ->
    samples_to_table(?DUMP_010_FIELDS, Samples, []);

samples_to_table(_Version, _Samples) ->
    {error, "unrecognized samples version"}.

%%--------------------------------------------------------------------

-spec samples_to_table([param()], [tuple()]|[], [tuple()]|[]) -> [tuple()]|[].
samples_to_table(_Params, [], List) ->
    List;

samples_to_table(Params, [{Mod, Fun, Data}|Rest], List) ->
    List2 = samples_to_table(Params, Mod, Fun, Data, 0, List),
    samples_to_table(Params, Rest, List2).

%%--------------------------------------------------------------------

-spec samples_to_table([param()], atom(), atom(), [list()]|[], integer(), [list()]|[])
                      -> [tuple()]|[].
samples_to_table(_Params, _Mod, _Fun, [], _Seq, List) ->
    List;

samples_to_table(Params, Mod, Fun, [Values|Rest], Seq, List) when is_list(Values) ->
    Records = [ {Mod, Fun, Seq, P, V} || {P, V} <- lists:zip(Params, Values) ],
    samples_to_table(Params, Mod, Fun, Rest, Seq+1, List++Records).

%%--------------------------------------------------------------------
