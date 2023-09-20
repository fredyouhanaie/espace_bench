%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% Works with the contents of a baseline data file generated by
%%% `rebar3_bench'.
%%%
%%% @end
%%% Created : 01 Sep 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(r3bench).

%% API exports
-export([main/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

-define(Log_level, notice).

-define(Version, "0.1.0").

-define(Opt_specs,
        [
         %%{Name,   ShortOpt,  LongOpt,    ArgSpec,        HelpMsg}
         {help,     $h,        "help",     undefined,      "Print help."},
         {version,  $v,        "version",  undefined,      "Print version."},
         {loglevel, $l,        "loglevel", {atom, notice}, "Set log level."}
        ]).

-define(Commands,
        [ {"dump",    "Dump the contents of FILE to stdout"},
          {"extract", "Extract the contents of FILE into DIR"},
          {"info",    "Print information about FILE"},
          {"table",   "Print contents of FILE in tabular form"}
        ]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    logger:set_primary_config(level, ?Log_level),

    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),

    ?LOG_DEBUG(#{func => ?FUNCTION_NAME, msg => "startup", args => Args} ),

    case getopt:parse(?Opt_specs, Args) of
        {error, {Reason, Data}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         reason => Reason,
                         data => Data}),
            usage();

        {ok, {Parsed, Rest}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         parsed => Parsed,
                         rest => Rest}),
            process_args(Parsed, Rest)
    end,

    timer:sleep(100),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-define(Process_opt(Opt, Action),
        case (proplists:get_value(Opt, Opts)) of
            true -> Action, true;
            _    -> false
        end).

-spec process_args(proplists:proplist(), list()) -> ok|error.
process_args(Opts, Args) ->
    logger:set_primary_config(level, proplists:get_value(loglevel, Opts)),

    ?Process_opt(version, io:format("Version ~p.~n", [?Version]))
        orelse ?Process_opt(help, usage())
        orelse process_command(Args).

%%--------------------------------------------------------------------

usage() ->
    io:format("Version ~p.~n", [?Version]),
    getopt:usage(?Opt_specs, atom_to_list(?MODULE), "command ...",
                 [ {"command", "command to execute, e.g. dump, table, info ..."} ]),
    [ io:format("  ~10s  ~s~n", [Cmd, Desc])
      || {Cmd, Desc} <- ?Commands ],
    ok.

%%--------------------------------------------------------------------

-spec process_command(list()) -> ok|error.
process_command([]) ->
    ?LOG_ERROR("command expected.~n", []),
    usage(),
    error;

process_command([Cmd|Args]) ->
    do_command(list_to_atom(Cmd), Args).

%%--------------------------------------------------------------------

-spec do_command(atom(), list()) -> ok|error.
do_command(dump, [File]) ->
    catch r3bench_utils:print(File),
    ok;

do_command(table, [File]) ->
    {_Version, Table} = r3bench_utils:table(File),
    catch print_table(Table),
    ok;

do_command(info, [File]) ->
    Info = r3bench_utils:info(File),
    io:format("Version:    ~p~n", [map_get(version  , Info)]),
    io:format("Parameters: ~p~n", [map_get(params   , Info)]),
    io:format("Modules:    ~p~n", [map_get(modules  , Info)]),
    io:format("Functions:  ~p~n", [map_get(functions, Info)]),
    ok;

do_command(Cmd, _) when Cmd == dump orelse Cmd == table ->
    ?LOG_ERROR("~p: single filename expected.~n", [Cmd]),
    error;

do_command(extract, [File, Dir]) ->
    r3bench_utils:save_map(r3bench_utils:gen_map(File), Dir);

do_command(extract, _Args) ->
    ?LOG_ERROR("extract: filename and directory expected.~n", []),
    error;

do_command(Cmd, _Args) ->
    ?LOG_ERROR("Unknown command (~p).~n", [Cmd]),
    usage(),
    error.

%%--------------------------------------------------------------------

print_table([]) ->
    ok;
print_table([Row|Rest]) ->
    io:format("~p.~n", [Row]),
    print_table(Rest).

%%====================================================================
