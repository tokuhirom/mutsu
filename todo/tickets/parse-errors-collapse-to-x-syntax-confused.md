# mutsu collapses most parse errors to `X::Syntax::Confused`, and only the native `throws-like` hides it

Eight roast assertions across seven whitelisted files ask `throws-like` for a
*specific* compile-time exception class. mutsu raises a generic
`X::Syntax::Confused` (or `X::AdHoc`) for all of them. They pass today only
because mutsu's **native** `throws-like` deliberately broadens the type check:
`src/runtime/test_functions/throws_like.rs` accepts any error whose message
contains `"Confused"` / `"parse error"` whenever the *expected* class starts with
`X::Syntax`, and has a similar widening for `X::Comp` / `X::Comp::Group`. The
real `Test.rakumod` compares `$_ ~~ $expected` and reports "right exception type
… FAILED", so every one of these regresses under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`).

**This is a mutsu bug, not a `Test` bug.** The fix is to raise the right class,
not to keep the native provider's leniency — and each of these is independent of
the others, so this is a volume ticket that parallelises.

## Measured 2026-08-28 (release build; `raku` column is the oracle)

| snippet (inside `EVAL`) | mutsu | raku | roast file / assertion |
| --- | --- | --- | --- |
| `@arr [0]` | `X::Syntax::Confused` | `X::Syntax::Missing` | `S02-lexical-conventions/minimal-whitespace.t` #1 |
| `42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` | `S02-lexical-conventions/minimal-whitespace.t` #17 |
| `say 42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` | `S02-lexical-conventions/minimal-whitespace.t` #18 |
| `"${$scalar}"` | `X::AdHoc` | `X::Obsolete` | `S03-operators/context.t` #27 |
| `"@{$array}"` | `X::AdHoc` | `X::Obsolete` | `S03-operators/context.t` #29 |
| `rt54804( 1, , 3, )` | `X::Syntax::Confused` | `X::Syntax::InfixInTermPosition` | `S06-signature/optional.t` #15 |
| `{my $foo; $^foo;}(1)` | `X::AdHoc` | `X::Redeclaration` | `S06-signature/positional-placeholders.t` #7 |
| `{*.{}}()` | `X::Syntax::Confused` | `X::Syntax::Malformed` | `S02-types/whatever.t` #67 |
| `'RT' ~~ m\c[SNOWMAN].\c[COMET]` | `X::Syntax::Confused` | `X::Comp::Group` | `S02-literals/quoting-unicode.t` #72 |

Note the two `X::Obsolete` rows: the *unquoted* forms (`${$scalar}`, `@{$array}`)
already raise `X::Obsolete` correctly — only the interpolated string forms fall
back to `X::AdHoc`, so the deprecated-P5-dereference detector exists and simply
does not run inside `qq` interpolation. That pair is probably the cheapest of
the nine.

## Not in this family (separately filed / already known)

- `S12-enums/misc.t` #26 — `X::Enum::NoValue` is raised with the right class but
  an empty `.enum` attribute (`Expected: Direction, Got: `).
- `S32-exceptions/misc2.t` #13-15 — `X::Placeholder::Mainline` is not raised at
  all; recorded as a separate gap in `todo/deep/vendor-real-test-module.md`
  (2026-08-28).
- `S02-lexical-conventions/comments.t` #41 — not an exception-class issue at all;
  see `todo/tickets/eval-write-to-outer-lexical-lost-inside-a-closure-or-routine.md`.

## Method note

Do not "fix" any of these by widening `throws_like.rs` further. The point of the
campaign is to retire the native provider, and every widening there is a
divergence that has to be paid back later.
