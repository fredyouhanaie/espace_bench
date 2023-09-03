#!/bin/sh

# run_bench.sh
#
# run rebar3_bench with increased process_limit.
#
# This is specifically for the eval runs, where 1000s of evals are run
# concurrently, and the default limit of 262144 is insufficient.


export ERL_FLAGS="+P $(( 32*262144 ))"
rebar3 bench "$@"
