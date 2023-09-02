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

-export([print/1]).

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
