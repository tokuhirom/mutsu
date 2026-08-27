# `INIT { $^c }` does not raise `X::Placeholder::Block`

A phaser block is not a routine, so a placeholder parameter (`$^c`) in one is
`X::Placeholder::Block`. mutsu enforces that for every phaser
`t/placeholder-scope-rejecting.t` covers -- `BEGIN`, `CHECK`, `PRE`, `CATCH`,
`CONTROL`, `once`, `try`, `react`, `loop`, `default`, `gather`, `supply`,
`start`, `sink`, `lazy`, `module`, `package`, `grammar` -- **except `INIT`**.

## Repro

```
$ mutsu -e 'try { EVAL q[INIT { $^c }] }; say $! ?? $!.^name !! "NONE"'
NONE
$ raku  -e 'try { EVAL q[INIT { $^c }] }; say $! ?? $!.^name !! "NONE"'
X::Placeholder::Block
```

`BEGIN { $^c }` is rejected correctly in mutsu, so the check exists and simply
does not cover the `INIT` arm.

## How it surfaces

`t/placeholder-scope-rejecting.t` subtest 13 (`INIT {} rejects a placeholder`)
fails under `MUTSU_REAL_TEST=1` and passes under mutsu's native `Test`
provider. The provider difference is only in how the code string is evaluated;
the gap itself is plain and reproduces with no Test module at all (above).

## Where to look

Whatever list of block kinds the placeholder-scope check consults -- grep for
the other phaser names alongside `X::Placeholder::Block`; `BEGIN` and `CHECK`
are already there and `INIT` needs adding beside them. Expect this to be a
one-line addition plus a pin; the surrounding machinery is already correct,
since 26 of the file's 27 subtests pass.
