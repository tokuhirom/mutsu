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

Pin: `t/missing-block-exception.t` (passes verbatim under `raku`).
