# `Q[[...]]` drops the leading nested bracket

A bracketing quote whose content *starts* with the same opening bracket loses
that bracket and its match:

```
$ mutsu -e 'say Q[[1]]'
1                       # raku: [1]
$ mutsu -e 'say Q[[1] 2]'
===SORRY!=== Error while compiling -e
```

The same content with anything before the nested bracket is fine, so nesting
itself works — only a *leading* nested delimiter is mishandled:

```
$ mutsu -e 'say Q[x[1]]'
x[1]                    # correct
```

`q[[1]]` and `Q<<1>>` show it too, so it is the bracketing-quote delimiter
scanner rather than anything specific to `Q`.

## Why it matters

Beyond the obvious string bug, this is a papercut in RakuAST work, where
`Q[...]` is the idiomatic way to hand a program to `.AST`. `Q[[+] 1, 2, 3].AST`
silently parses a *different program* than the one written — it was first hit
while testing reduction lowering, where the workaround was to switch the test to
`Q{[+] 1, 2, 3}`. A silently-wrong parse is worse than an error here, because a
dual-oracle test written that way compares two different programs.

## Where to look

The bracketing-quote scanner that finds the closing delimiter and counts nested
pairs — `src/parser/` quote handling. A plausible shape for the bug is that the
scanner consumes the opening delimiter, then treats the immediately-following
identical character as the *closing* one (or as a second opening delimiter for
the outer quote) instead of starting the nesting count at the first content
character.

## Minimal repro

```
say Q[[1]];      # expected: [1]      got: 1
say Q[[1] 2];    # expected: [1] 2    got: a compile error
say q[[1]];      # expected: [1]      got: 1
say Q[x[1]];     # expected: x[1]     got: x[1]  (correct)
```
