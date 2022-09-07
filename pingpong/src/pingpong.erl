-module(pingpong).

%% API exports
-export([main/1]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

-define(Log_level, notice).

-define(Version, "0.1.0").

-define(Opt_specs,
        [
         %%{Name,   ShortOpt,  LongOpt,    ArgSpec,          HelpMsg}
         {help,     $h,        "help",     undefined,        "Print help."},
         {version,  $v,        "version",  undefined,        "Print version."},
         {loglevel, $l,        "loglevel", {atom, notice},   "Set log level."}
        ]).

-define(Process_opt(Opt, Action),
        case (proplists:get_value(Opt, Opts)) of
            true -> Action, true;
            _    -> false
        end).

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

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

usage() ->
    io:format("Version ~p.~n", [?Version]),
    getopt:usage(?Opt_specs, atom_to_list(?MODULE), "[PINGS]",
                 [ {"PINGS", "number of ping pairs (default 20_000)"} ]).

%%--------------------------------------------------------------------

-spec process_args(proplists:proplist(), list()) -> ok|error.
process_args(Opts, Args) ->
    logger:set_primary_config(level, proplists:get_value(loglevel, Opts)),

    %% -v will only print version, and ignore rest of args
    %% -h will include version, and ignore rest of args
    %% only process command if no -v/-h
    ?Process_opt(version, io:format("Version ~p.~n", [?Version]))
        orelse ?Process_opt(help, usage())
        orelse process_command(Args).

%%--------------------------------------------------------------------

process_command([]) ->
    pingpong1:start();

process_command([Arg_limit]) ->
    ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                 limit => Arg_limit}),
    try list_to_integer(Arg_limit) of
        Limit -> pingpong1:start(Limit)
    catch
        error:E ->
            ?LOG_ERROR(#{func => ?FUNCTION_NAME, error => E}),
            usage()
    end;

process_command(Args) ->
    io:format("too many arguments (~p)~n", [Args]),
    usage().

%%--------------------------------------------------------------------
