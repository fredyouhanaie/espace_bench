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
-module(r3bench_utils).

-export([read_file/1, print/1, table/1, load_ets/1, info/1, gen_map/1]).
-export([save_map/2, ministat/4]).

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

-type samples_map() :: #{module() =>
                             #{function() =>
                                   #{param() => [float()]}
                              }}.
%% The samples as a map.

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
%% Some sanity checks are performed on the file contents. In case of
%% an error, `{error, Reason}' is returned, otherwise we return the
%% `{Version, Samples}' tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_file(file:name_all()) -> dumpfile() | {error, term()}.
read_file(Filename) ->
    case file:read_file(Filename) of
        Error = {error, _} ->
            Error;
        {ok, Data} ->
            case Data of
                <<>> ->
                    {error, badarg};
                _ ->
                    case erlang:binary_to_term(Data) of
                        Error = {error, _} ->
                            Error;
                        {Version, Samples}
                          when is_list(Version) andalso is_list(Samples) ->
                            {Version, Samples};
                        _ ->
                            {error, invalid_file}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Return a map of the baseline data hierarchy.
%%
%% The map will contain one sub-map for each module name in the
%% baseline data.
%%
%% Each module sub-map will contain one sub-map for each function
%% within that module.
%%
%% Each function sub-map will contain one sub-map for each data
%% parameter. The paramater names are based on the version of baseline
%% file, see the `param()' data type for details.
%%
%% Each param element will contain the corrensponding benchmarks as a
%% list of floats.
%%
%% For now, in the interest of simplicity, we are generating the
%% nested map from the `table()' structure, although in future this
%% may change.
%%
%% @end
%%--------------------------------------------------------------------
-spec gen_map(file:name_all()) -> samples_map().
gen_map(File) ->
    {_Version, Table} = table(File),
    table_to_map(Table, #{}).

%%--------------------------------------------------------------------

ministat(Map, Conf_level, {M1,F1,P1}, {M2,F2,P2}) ->
    DS_1 = map_to_ds(Map, M1, F1, P1),
    DS_2 = map_to_ds(Map, M2, F2, P2),
    eministat:x(Conf_level, DS_1, DS_2).

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
    maps:map(fun (_K, V) -> lists:usort(V) end, Map);

unzip([{Mod, Fun, _, Par, _}|Rest], Map) ->
    Map1 = #{modules => Mod, functions => Fun, params => Par},
    Map2 = maps:merge_with(fun (_K, L, X) -> [X|L] end, Map, Map1),
    unzip(Rest, Map2).

%%--------------------------------------------------------------------
%% @doc Convert a table of benchmarks to a map of maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec table_to_map(table(), samples_map()) -> samples_map().
table_to_map([], Map) ->
    Map;

table_to_map([ {Mod, Fun, _Seq, Par, Value} | Rest ], Map) ->
    Mod_map = maps:get(Mod, Map, #{}),

    Fun_map  = maps:get(Fun, Mod_map, #{}),
    Fun_map2 = maps:update_with(Par, fun (Vs) -> Vs++[Value] end, [], Fun_map),

    Mod_map2 = maps:put(Fun, Fun_map2, Mod_map),

    Map2 = maps:put(Mod, Mod_map2, Map),

    table_to_map(Rest, Map2).

%%--------------------------------------------------------------------
%% @doc Extract and save samples from a map into data files.
%%
%% Given a map of samples, as produced with `gen_map/1', save the data
%% in a directory hierarch, that correspond to the map structure.
%%
%% Basically, we will have directories for module names, where each
%% contains directories for the functions. Within each function
%% directory there will be one file per benchmark paramater containing
%% the measurements for that function.
%%
%% The whole set of directories will be in the `Dir' pathname, which
%% should be either non-existent or an empty directory.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_map(map(), file:name_all()) -> ok.
save_map(Map, Dir) ->
    case file:make_dir(Dir) of
        ok ->
            save_mod(maps:iterator(Map), Dir);
        {error, eexist} ->
            case file:list_dir_all(Dir) of
                {ok, []} ->
                    save_mod(maps:iterator(Map), Dir);
                {ok, _} ->
                    {error, not_empty};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Extract and save samples from a map into data files.
%%
%% Save the measurements corresponding to the modules in their own
%% directories, relative to `Dir'.
%%
%% The directory path `Dir' should exist, and is expected to
%% correspond to the set of benchmarks that include the modules.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_mod(maps:iterator(), file:name_all()) -> ok.
save_mod(Iter, Dir) ->
    case maps:next(Iter) of
        none ->
            ok;
        {Mod, Fun_map, Iter2} ->
            Mod_dir = filename:join(Dir, Mod),
            ok = file:make_dir(Mod_dir),
            save_funs(maps:iterator(Fun_map), Mod_dir),
            save_mod(Iter2, Dir)
    end.

%%--------------------------------------------------------------------
%% @doc Save functions into individual directories.
%%
%% Save the measurements corresponding to functions in their own
%% directories, relative to `Dir'.
%%
%% The directory path `Dir' is expected to correspond to the parent
%% module of the functions.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_funs(maps:iterator(), file:name_all()) -> ok.
save_funs(Iter, Dir) ->
    case maps:next(Iter) of
        none ->
            ok;
        {Fun, Par_map, Iter2} ->
            Fun_dir = filename:join(Dir, Fun),
            ok = file:make_dir(Fun_dir),
            save_pars(maps:iterator(Par_map), Fun_dir),
            save_funs(Iter2, Dir)
    end.

%%--------------------------------------------------------------------
%% @doc Save the benchmark mesasurements into text files.
%%
%% Save the measurements corresponding to a map of `{Par,Val_list}'
%% element in their own files. The files will be within the `Dir'
%% directory.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_pars(maps:iterator(), file:name_all()) -> ok.
save_pars(Iter, Dir) ->
    case maps:next(Iter) of
        none ->
            ok;
        {Par, Par_values, Iter2} ->
            Par_file = atom_to_list(Par),
            {ok, Par_dev} = file:open(filename:join(Dir, Par_file), [write]),
            [ io:format(Par_dev, "~w~n", [Val]) || Val <- Par_values ],
            file:close(Par_dev),
            save_pars(Iter2, Dir)
    end.

%%--------------------------------------------------------------------

map_to_ds(Map, Mod, Fun, Par) ->
    Data = map_get(Par, map_get(Fun, map_get(Mod, Map))),
    MFP0 = [ atom_to_list(X) || X <- [Mod, Fun, Par] ],
    Name = string:join(MFP0, "__"),
    eministat_ds:from_list(Name, Data).

%%--------------------------------------------------------------------
