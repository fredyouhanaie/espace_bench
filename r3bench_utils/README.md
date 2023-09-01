# r3bench_utils

A set of utilities for working with the files produced by `rebar3_bench`.

The escript `r3bench_dump` will dump the contents of the baseline data
from a `rebar3_bench` run.

Basically, the intention is to run `rebar3 bench` multiple times for
different versions of `espace`, or any other application, and compare
the collected data.

Dumping the data into a form readable by other data analysis
applications is just the start.

## Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/r3bench_dump /path_to_project/_build/test/bench/_tip

---
