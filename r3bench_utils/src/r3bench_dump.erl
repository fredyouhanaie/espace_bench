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

-export([print/1, table/1, load_ets/1, info/1]).

%%--------------------------------------------------------------------

%% below definitions taken from rebar3_bench:rebar_bench_prv.erl
%%
-define(DUMP_FMT_VERSION, [0, 1, 0]).
-define(DUMP_010_FIELDS,  [wall_time, memory, reductions]).

%%--------------------------------------------------------------------

-type param() :: wall_time | memory | reductions.
%% The parameter names (based on dump file version 0.1.0)

-type dumpfile() :: {version(), [samples()]}.
%% The unpacked dumpfile

-type version() :: [integer()].
%% The current version of the dump file is `[0, 1, 0]'.

-type samples() :: {module(), function(), [measurements()]}.
%% The data element of each dumpfile

-type measurements() :: [float()].
%% The current version contains the measurements for `wall_time',
%% `memory' and `reductions', respectively.

-type table() :: []|[table_row()].
%% The table of samples, expanded version of the raw samples.

-type table_row() :: {module(), function(), integer(), param(), float()}.
%% Each row of the table, for example as loaded into an ETS table.

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
    {Version, Samples} = read_file(Filename),
    io:format("version=~p.~n", [Version]),
    [ io:format("Mod=~p, Fun=~p, Data=~p.~n", [M, F, D])
      || {M, F, D} <- Samples ],
    ok.

%%--------------------------------------------------------------------
%% @doc return a list of measurements.
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
-spec table(file:name_all()) -> {version(), table()}.
table(Filename) ->
    {Version, Samples} = read_file(Filename),
    {Version, samples_to_table(Version, Samples)}.

%%--------------------------------------------------------------------
%% @doc load the samples from `Filename' into a new ETS table.
%%
%% A new ETS table is created and the tabid returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_ets(file:name_all()) -> ets:table().
load_ets(Filename) ->
    {_Version, Samples} = table(Filename),
    Tab_id = ets:new(r3bench, [bag]),
    ets:insert(Tab_id, Samples),
    Tab_id.

%%--------------------------------------------------------------------
%% @doc return summary information about the data file.
%%
%% A `map' of the base information is returned, namely `version',
%% `modules', `functions' and `params'.
%%
%% @end
%%--------------------------------------------------------------------
-spec info(file:name_all()) -> map().
info(Filename) ->
    {Version, Samples} = table(Filename),
    maps:put(version, Version, unzip(Samples)).

%%--------------------------------------------------------------------
%% @doc read and unpack a dump file and return the raw samples.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_file(file:name_all()) -> dumpfile().
read_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {Version, Samples} = erlang:binary_to_term(Data),
    {Version, Samples}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc convert the raw samples structure to a list of lists.
%%
%% see types `table()' and `table_row()' for further details.
%%
%% @end
%%--------------------------------------------------------------------
-spec samples_to_table(version(), [samples()]|[]) -> table().
samples_to_table(?DUMP_FMT_VERSION, Samples) ->
    samples_to_table(?DUMP_010_FIELDS, Samples, []);

samples_to_table(_Version, _Samples) ->
    {error, "unrecognized samples version"}.

%%--------------------------------------------------------------------
%% @doc scan all the data and return a list of tuples, one per
%% measurment.
%%
%% @end
%%--------------------------------------------------------------------
-spec samples_to_table([param()], [samples()]|[], table()) -> table().
samples_to_table(_Params, [], List) ->
    List;

samples_to_table(Params, [{Mod, Fun, Data}|Rest], List) ->
    List2 = samples_to_table(Params, Mod, Fun, Data, 0, List),
    samples_to_table(Params, Rest, List2).

%%--------------------------------------------------------------------
%% @doc scan the measurements of the supplied `Mod' and `Fun'.
%%
%% For each list of measurements, currently 3, we generate a list of 3
%% tuples and append them to the accumulator `List', i.e.
%%
%% see `table_row()' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec samples_to_table([param()], module(), function(),
                       [measurements()]|[], integer(), table())
                      -> table().
samples_to_table(_Params, _Mod, _Fun, [], _Seq, List) ->
    List;

samples_to_table(Params, Mod, Fun, [Values|Rest], Seq, List) when is_list(Values) ->
    Records = [ {Mod, Fun, Seq, P, V} || {P, V} <- lists:zip(Params, Values) ],
    samples_to_table(Params, Mod, Fun, Rest, Seq+1, List++Records).

%% --------------------------------------------------------------------
%% @doc extract map of modules, functions and params from a table of
%% samples.
%%
%% @end
%%--------------------------------------------------------------------
-spec unzip(table()) -> map().
unzip(Samples) ->
    unzip(Samples, #{modules => [], functions => [], params => []}).

%% --------------------------------------------------------------------
%% @doc extract map of modules, functions and params from a table of
%% samples.
%%
%% We return three lists of unique names of modules, functions and
%% parameters.
%%
%% @end
%%--------------------------------------------------------------------
-spec unzip(table(), map()) -> map().
unzip([], Map) ->
    maps:map(fun (K, V) -> lists:usort(V) end, Map);

unzip([{Mod, Fun, _, Par, _}|Rest], Map) ->
    Map1 = #{modules => Mod, functions => Fun, params => Par},
    Map2 = maps:merge_with(fun (K, L, X) -> [X|L] end, Map, Map1),
    unzip(Rest, Map2).

%%--------------------------------------------------------------------
