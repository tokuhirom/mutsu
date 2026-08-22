# Colon-call's trailing `.method` binds to the last argument, not the whole call

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/syntax.rakudoc:1091`).

## Root cause

In Raku, a colon-call's argument list is a low-precedence listop, so a `.method` written
right after the last argument (with whitespace, no dot-call chaining syntax) binds to
*that last argument*, not to the overall method-call's result. Confirmed via
`raku --target=ast`: `$band.substr: 0, 3  .uc` compiles to
`callmethod substr($band, 0, callmethod uc(3))` — i.e. `.substr(0, 3.uc)`, where `3.uc`
stringifies `3` to `"3"` (uc is a no-op on digits) so the effective call is just
`$band.substr(0, "3")`, producing `"Foo"` (no uppercasing at all — this is Raku's own
"trap"/gotcha the doc section is illustrating).

mutsu instead returns the entire original string unmodified (`"Foo Fighters"`), meaning it
neither performs the substr correctly nor applies `.uc` to the last argument the way raku's
parser does — the whole call appears to be mis-parsed or mis-evaluated for this
no-parens-colon-call-with-trailing-method shape.

## Minimal repro

```raku
my $band = 'Foo Fighters';
say $band.substr( 0, 3 ).uc; # OUTPUT: «FOO␤»   -- parenthesized: mutsu already OK
say $band.substr: 0, 3  .uc; # OUTPUT: «Foo␤»   -- colon-call: mutsu gives "Foo Fighters"
```

- `raku`: `FOO` then `Foo`
- `mutsu`: `FOO` (correct) then `Foo Fighters` (wrong — should be `Foo`)

## Affected files (starting point)

- Parser: colon-call argument-list parsing (`.method: arg1, arg2 ...`) — grep for the
  colon-call listop precedence handling and confirm whether a trailing `.method` after the
  last argument is currently attached to the whole call result instead of the last
  argument expression. Compare `--dump-ast` output for this repro against raku's
  `--target=ast` (shown above) to see where mutsu's parse tree diverges.
