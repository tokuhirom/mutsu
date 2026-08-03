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

Three things had to give way for it, all of them found by roast rather than
predicted:

- **mutsu's native `throws-like` recognised a parse failure by the words "parse
  error" in its message.** A failure the parser diagnoses precisely says only
  what is wrong ("Missing block"), so that substring test stopped firing and
  `roast/S32-exceptions/misc2.t` / `roast/S06-operator-overloading/sub.t` lost
  the leniency that lets any parse failure match an `X::Syntax` type when no
  structured exception is attached. It now reads the structured parse `code`
  instead, the way its own `X::Comp` branch already did.
- **"Missing block" counts as a diagnosis only when the block was the *primary*
  expectation** — the first alternative at that position. It is the weakest
  thing the parser can say, since a block is an alternative almost everywhere:
  `say 1 ]` fails with a hundred alternatives of which "block" is merely one,
  and rakudo calls that `X::Syntax::Confused`. Any *other* named class also
  outranks it, so `sub twigil:<@>() { }` keeps the
  `X::Syntax::Extension::Category` a sibling alternative diagnosed.
- **The failure position had to be carried through.** The statement-list loop
  supplies it when it wraps a failed statement's error; propagating a classified
  error instead skipped the wrapper, so `parse_program` had no line or column to
  report and the CLI lost its `------>` snippet. The propagation now fills the
  position in.

The rendered message drops the class prefix, so a `===SORRY!===` reads
`Missing block` exactly as rakudo's does; `$!.^name` is where the class shows up.

Pin: `t/missing-block-exception.t` (passes verbatim under `raku`).
