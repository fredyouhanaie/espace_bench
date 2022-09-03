# `espace_bench`

Scripts to run [micro]benchmarks against
[espace](https://github.com/fredyouhanaie/espace).

This repo was part of the `espace` project, however, it has been
separated from the main repository in order to enable running
[micro]benchmarks against multiple versions of `espace`.

In addition to generating microbenchmarks, other `espace` application
benchmarks are also being included here.

## using `rebar3_bench`

Currently we use the
[rebar3_bench](https://github.com/seriyps/rebar3_bench) plugin. In due
course other methods will be added here.

### Running the benchmarks

To run the benchmarks, you will need a copy of the repo, and the
latest copy of [rebar3](https://rebar3.org/):

```
git clone https://github.com/fredyouhanaie/espace_bench
cd espace_bench
```

WARNING: The command below will run all the benchmarks, which will
take a lot of CPU/Memory resources. It will also produce errors when
running the `eval` benchmarks. See the _Process limit_ section for
explanation:

```
rebar3 bench ## DO NOT RUN THIS, read the above para first!
```

You can restrict the run to a subset of benchmarks, e.g.

```
rebar3 bench -b bench_out_1
```

You will find a list of available microbenchmarks, named `bench_*`, in
the `test/bench_espace_1.erl` file.

### Process limit

The way `rebar3_bench` works is to run the benchmark functions 1000s
of times. This works fine for most functions. However, since `eval`
calls `spawn`, that causes the number of processes to exceed the
current default limit of 262144. This can be overcome by setting a
larger limit with the `+P` emulator flag. This is achieved
automatically with the `run_bench.sh` helper script. So, it's best to
use the script to run the benchmarks:

```
./run_bench.sh [ REBAR3_BENCH options ... ]
```

### Benchmarking other `espace` versions

The default version of `espace` benchmarked is the latest found on
[hex](https://hex.pm/packages/espace).

In order to run microbenchmark(s) against specific versions of
`espace`, you can use the
[`_checkouts`](https://rebar3.readme.io/docs/dependencies#checkout-dependencies)
facility provided by rebar3. For example, assuming that `espace` and
`espace_bench` have been cloned in the same parent directory, to run
two benchmarks for `REV1` and `REV2`:

```
# prepare to use _checkouts
#
cd espace_bench
mkdir _checkouts
cd  _checkouts
ln -sv ../../espace espace

# run the microbenchmarks against REV1
#
cd ../../espace
git checkout REV1
cd ../espace_bench
./run_bench.sh -b bench_out_1

# run the microbenchmarks against REV2
#
cd ../espace
git checkout REV2
cd ../espace_bench
./run_bench.sh -b bench_out_1

```
