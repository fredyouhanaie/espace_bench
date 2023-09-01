# r3bench_utils

A set of utilities for working with the files produced by `rebar3_bench`.

The escript `r3bench_dump` will dump the contents of the baseline data
from a `rebar3_bench` run.

## Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/r3bench_dump /path_to_project/_build/test/bench/_tip

---
