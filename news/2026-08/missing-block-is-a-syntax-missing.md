# A required block that is not there is `X::Syntax::Missing`

rakudo answers "a block was required here and I did not find one" with
`X::Syntax::Missing` (`what => 'block'`, rendered `Missing block`), not with its
catch-all `X::Syntax::Confused`. It uses the same diagnosis for the opening
brace and the closing one:

```
if 1; 2            → X::Syntax::Missing | Missing block
sub foo-($x) { }   → X::Syntax::Missing | Missing block
{my $x = 2;        → X::Syntax::Missing | Missing block
```

mutsu reported the generic `expected '{'` / `expected '}'` alternation instead,
so its exception classed as `X::Syntax::Confused` and every
`throws-like …, X::Syntax::Missing` failed on the class even though the parse
had correctly rejected the source.

`block()` and `block_inner()` now spell that expectation in the
`"X::Type: text"` convention, which
`news/2026-08/parse-error-keeps-its-exception-class.md` carries out to `$!`
intact. Nothing else changes: the expectation is still a recoverable
alternative, so a block that *is* there parses exactly as before, and a failure
that never considered a block still renders "Confused."

Under `MUTSU_REAL_TEST=1` this closes `roast/S04-statements/if.t` and
`roast/S02-names/identifier.t`, and moves `roast/S04-statements/terminator.t` on
to its next gap (`my $x =` wants `X::Syntax::Malformed`).
`roast/S02-lexical-conventions/minimal-whitespace.t` is unaffected — its
`@arr [0]` fails before any block alternative is reached, and rakudo's
"Missing block" there comes from a different rule.

Two things had to give way for it:

- **`X::Syntax::Missing` ranks last among classified diagnoses.** "A block was
  required here" is the weakest thing the parser can say — a block is an
  alternative almost everywhere — so any other named class describes the
  construct better. Without the ranking, `sub twigil:<@>() { }` reported
  `X::Syntax::Missing` instead of the `X::Syntax::Extension::Category` a sibling
  alternative had diagnosed.
- **mutsu's native `throws-like` recognised a parse failure by the words "parse
  error" in its message.** A failure the parser diagnoses precisely says only
  what is wrong ("Missing block"), so that substring test stopped firing and
  `roast/S32-exceptions/misc2.t` / `roast/S06-operator-overloading/sub.t` lost
  the leniency that lets any parse failure match an `X::Syntax` type when no
  structured exception is attached. It now reads the structured parse `code`
  instead, the way its own `X::Comp` branch already did.

Pin: `t/missing-block-exception.t` (passes verbatim under `raku`).
