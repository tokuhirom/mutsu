# `t/io-path-methods.t` builds its tree in a per-process temp directory

`t/io-path-methods.t` used to build its fixture in a **fixed** path under
`tmp/`, so it raced against any concurrent copy of itself:

```raku
my $base = "tmp/io-path-methods-regression".IO;
mkdir $base;
mkdir $real;
symlink $real, $link;
```

Two processes running the file at once — two `prove` invocations, or a
`make test` overlapping another — raced on the same `mkdir`/`symlink`, and
whichever lost saw a half-built tree. It was first observed on 2026-09-07 as
"planned 8, ran 6" (the two `resolve` rows) under a `prove` run, passing 8/8
immediately afterwards when run alone. A stale copy left by an interrupted run
reproduced the same failure with no concurrency at all — the same shape as the
`temp-file-RT-126006-test` leftover that `make roast` removes before starting.

## The fix

The tree now gets a per-process-unique name and an unconditional teardown
(`t/io-path-methods.t:23`):

```raku
my $base = "tmp/io-path-methods-regression-$*PID".IO;
```

with a `LEAVE { ... }` that removes it, so nothing is left behind for the next
run to trip over. Verified by running two concurrent
`prove -e target/debug/mutsu t/io-path-methods.t` invocations: both report
`All tests successful. Files=1, Tests=8`.

This is the directory twin of the port-collision fix CLAUDE.md records in its
"De-flaked" list for `t/io-socket-recv-limit.t`. The rule generalizes: **never
hardcode a shared name in a new test** — not a port, and not a path.

## Neighbourhood, as of 2026-09-07

Of the `t/*.t` files that still build a literal path under `tmp/`, only
`t/seektype-enum.t:26` (`tmp/seektype-enum-test.txt`) and
`t/io-path-raku-class-roundtrip.t:14`
(`tmp/io-path-raku-roundtrip-test.txt`) remain, and both only *write a single
file* — the lower-risk class, since there is no directory tree or symlink to
observe half-built. They are worth a one-line follow-up but were deliberately
not bundled into this fix.
