# `pingpong`

A ping-pong benchmark based on the `pingpong' C-Linda program in the
following paper:

> Carriero, Nicholas, and David Gelernter. ‘The S/Net’s Linda
> Kernel’. ACM Transactions on Computer Systems 4, no. 2 (May 1986):
> 110–29. https://doi.org/10.1145/214419.214420.

## Running the benchmark

`rabar3` is used for building and running the benchmark. To run the
benchmark within the shell

    $ rebar3 shell
    1> pingpong1:start().
    Pairs=20000, Time=3.8453 sec, Pairs/sec=5201.154656333706.
    2> [ pingpong1:start() || _ <- [1,2,3] ].
    Pairs=20000, Time=1.472577 sec, Pairs/sec=13581.632743143482.
    Pairs=20000, Time=1.407352 sec, Pairs/sec=14211.085783798226.
    Pairs=20000, Time=1.37389 sec, Pairs/sec=14557.206180989744.
    [ok,ok,ok]
