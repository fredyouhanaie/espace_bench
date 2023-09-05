# `pingpong`

A ping-pong benchmark based on the `pingpong` C-Linda program in the
following paper:

> Carriero, Nicholas, and David Gelernter. ‘The S/Net’s Linda
> Kernel’. ACM Transactions on Computer Systems 4, no. 2 (May 1986):
> 110–29. <https://doi.org/10.1145/214419.214420>

Basically, two workers are started, namely `ping` and `pong`.

* `ping` will repeatedly wait/block for a `{ping}` tuple, once
  received, it will output a `{pong}` tuple.

* `pong` will repeatedly output a `{ping}` tuple, then wait/block for
  a `{pong}` tuple.

* The interaction stops after 20_000 pings (default) or the count
  passed to `pingpong1:start/1`, or the escript command.

## Running the benchmark

`rabar3` is used for building and running the benchmark.

To run the benchmark from within the erlang shell

```shell
$ rebar3 shell
1> pingpong1:start().
Pairs=20000, Time=3.8453 sec, Pairs/sec=5201.154656333706.

2> [ pingpong1:start() || _ <- [1,2,3] ].
Pairs=20000, Time=1.472577 sec, Pairs/sec=13581.632743143482.
Pairs=20000, Time=1.407352 sec, Pairs/sec=14211.085783798226.
Pairs=20000, Time=1.37389 sec, Pairs/sec=14557.206180989744.
[ok,ok,ok]
3>
```

To run the benchmark escript:

```shell
$ rebar3 escriptize
$ ./_build/default/bin/pingpong
Pairs=20000, Time=1.221945 sec, Pairs/sec=16367.348775926903.

$ for i in 1 2 3; do ./_build/default/bin/pingpong ; done
Pairs=20000, Time=1.504959 sec, Pairs/sec=13289.39858162249.
Pairs=20000, Time=1.258232 sec, Pairs/sec=15895.319782043374.
Pairs=20000, Time=1.269835 sec, Pairs/sec=15750.07776600897.
```

---
