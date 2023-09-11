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

-define(KV_battle_bench, "test/kv_battle-20230908-1.dat").
-define(Info_kv_battle, #{functions =>
                              [bench_dict,bench_ets,bench_gb_tree,bench_keyfind,
                               bench_maps,bench_orddict,bench_proplists],
                          modules => [bench_kv],
                          version => [0,1,0],
                          params => [memory,reductions,wall_time]}).

file_read_test_() ->
    {"File read",
     [ {"info", ?_assertEqual(?Info_kv_battle, r3bench_dump:info(?KV_battle_bench)) }
     ]}.

%%--------------------------------------------------------------------
