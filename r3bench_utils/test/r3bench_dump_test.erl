%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% Unit tests for `r3bench_dump'
%%%
%%% @end
%%% Created : 11 Sep 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(r3bench_dump_test).

%%--------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

file_access_test_() ->
    {"File access",
     [ {"non-existent file", ?_assertEqual({error, enoent}, r3bench_dump:read_file("nonexistent_file.dat"))},
       {"empty file",        ?_assertEqual({error, badarg}, r3bench_dump:read_file("test/empty_file.dat"))},
       {"invalid file",      ?_assertException(error, badarg, r3bench_dump:read_file("test/bad_file.dat"))}
     ]}.

%%--------------------------------------------------------------------
