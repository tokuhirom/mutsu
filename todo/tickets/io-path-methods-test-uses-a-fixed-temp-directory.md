# `t/io-path-methods.t` collides with itself on a fixed `tmp/` directory

Observed 2026-09-07: the file failed under a `prove` run ("planned 8, ran 6",
the two `resolve` rows) and passed 8/8 immediately afterwards when run alone.

## Root cause

```raku
my $base = "tmp/io-path-methods-regression".IO;
...
mkdir $base;
mkdir $real;
symlink $real, $link;
```

The directory name is FIXED, so two processes running this file at once — two
`prove` invocations, or a `make test` overlapping another — race on the same
`mkdir`/`symlink`, and whichever loses sees a half-built tree. A stale copy left
behind by an interrupted run reproduces the same failure on the next run with no
concurrency at all (the same shape as the `temp-file-RT-126006-test` leftover
`make roast` removes before starting).

## Fix

Give the tree a per-process-unique name (`$*PID`, or `make-temp-dir` from
`roast/packages/Test-Helpers/lib/Test/Util.rakumod`, which several `t/` files
already use), and remove it in a `LEAVE`/`END` so nothing is left behind. This
is the directory twin of the port-collision fix recorded in CLAUDE.md's
"De-flaked" list for `t/io-socket-recv-limit.t`: "never hardcode a port in a new
test" — the same rule applies to a shared path.

## Neighbourhood to check when fixing

Other `t/` files that build a fixed path under `tmp/`
(`grep -l '"tmp/' t/*.t`) — several exist and have the same exposure; the ones
that only WRITE a file are lower-risk than this one, which builds a directory
tree and a symlink.
