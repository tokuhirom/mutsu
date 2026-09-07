# `t/io-path-methods.t` no longer races with itself

The file built its symlink tree at a **fixed** path:

```raku
my $base = "tmp/io-path-methods-regression".IO;
```

so two processes running it at once raced on the same `mkdir`/`symlink` and
whichever lost saw a half-built tree — "planned 8, ran 6", the two `resolve`
rows. Measured: six concurrent `prove` runs of the old file failed 3 of 6; the
fixed file passes 6 of 6 and leaves nothing behind. Standalone it always passed,
which is what made the failure look like noise.

It was the lone outlier. Every other `t/` file that builds a tree under `tmp/`
— `t/native-io-path-{comb,content-read,fs-mutate,fs-stat,open,two-path}.t`,
`t/compunit-need-protocol.t` — already names it per-`$*PID`; this one now does
too. That is the directory twin of the hardcoded-port collision CLAUDE.md
records for `t/io-socket-recv-limit.t` ("never hardcode a port in a new test"),
and a unique name also means a leftover from an interrupted run can never be
mistaken for this run's tree — so the stale-cleanup prologue the file carried
is gone.

The teardown moved into a `LEAVE` block, so a failure anywhere in the file
cannot leave the tree behind either. (`dies-ok` already swallowed its own
exception, but nothing else did.)
