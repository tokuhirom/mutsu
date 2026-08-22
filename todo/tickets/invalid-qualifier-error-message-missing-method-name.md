# `X::Method::InvalidQualifier` message drops the method name (says "a method" instead of "method NAME")

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/X/Method/InvalidQualifier.rakudoc:14`).

## Repro

```raku
1.Str::split(/a/);
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Method::InvalidQualifier: Cannot dispatch to method split on Str because it is not inherited or done by Int`
- `mutsu` (`target/debug/mutsu`): `X::Method::InvalidQualifier: Cannot dispatch to a method on Str because it is not inherited or done by Int`

Verified directly with `raku -e` / `target/debug/mutsu -e` on this exact snippet.

The exception type matches (`X::Method::InvalidQualifier`, correctly thrown — this is
the same qualified-method-dispatch machinery that `operators.rakudoc` [17] / #5124
fixed for the *successful* dispatch case). The bug here is narrower: the thrown
exception's message substitutes a generic "a method" for the actual method name
("split") that was being dispatched. This is not mere wording drift (which would be
excluded per this ledger's "message text differs, meaning matches" rule) — it's a
missing piece of information that Rakudo's message includes and mutsu's doesn't,
which could matter for scripts that pattern-match on the exception text.

## Root cause hypothesis

Wherever `X::Method::InvalidQualifier` is constructed/thrown (grep for
`"InvalidQualifier"` in `src/runtime/`), the message-building code isn't passed (or
isn't interpolating) the method name that was being looked up — likely a hardcoded "a
method" string where the real method name should be substituted.

## Affected files (starting point)

- `src/runtime/` — grep for `"InvalidQualifier"` to find the exception construction
  site and thread the method name through to the message.
