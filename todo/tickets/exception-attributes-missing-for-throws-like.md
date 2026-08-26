# mutsu's structured exceptions are missing rakudo attributes, so `throws-like` skips those matchers

Split out of the `throws-like` vacuous-matcher fix
(`news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md`).

## What is happening

`throws-like` now runs every named matcher whose value mutsu can actually
produce (stored attribute, a method on the exception, or the carrier text for
`message`/`gist`). When mutsu can produce *nothing* for the name, the check is
skipped and the subtest announces it:

```
# SKIPPED matcher '.multiness': mutsu's X::Anon::Multi carries no such attribute
```

Rakudo does not skip: `Test.rakumod` calls `$x."$k"()`, so a missing attribute
raises `X::Method::NotFound` and aborts the subtest. mutsu's skip keeps the
assertion from failing spuriously on what is really a *missing-attribute* bug
rather than a bad test — but it does mean those particular assertions still
assert nothing. The skip line is deliberately loud so they cannot be forgotten.

Remove the `unresolvable` bucket in
`src/runtime/test_functions/throws_like.rs` once the attributes below exist.

## The precise remaining list

Each entry was verified against `raku` and is a genuine mutsu gap. All of these
subtests currently emit a SKIPPED line instead of a check.

| roast file | matcher | what mutsu produces today |
|---|---|---|
| `S32-exceptions/misc2.t:116` | `X::Syntax::Variable::Numeric, what => 'parameter'` | raises `X::Syntax::Confused`, no `.what` |
| `S32-exceptions/misc2.t:240` | `X::Anon::Multi, multiness => 'multi'` | correct class, no `.multiness` |
| `S32-exceptions/misc2.t:241` | `X::Anon::Multi, multiness => 'proto'` | correct class, no `.multiness` |
| `S32-exceptions/misc2.t:242` | `X::Anon::Multi, routine-type => 'method'` | correct class, no `.routine-type` |
| `S32-exceptions/misc2.t:250` | `X::Syntax::Number::RadixOutOfRange, radix => 45` | raises `X::Syntax::Confused`, no `.radix` |
| `S32-exceptions/misc2.t:277` | `X::Syntax::Missing, what => /constant/` | raises `X::Syntax::Confused`, no `.what` |
| `S32-exceptions/misc2.t:304-306` | `X::Method::InvalidQualifier, method`/`invocant`/`qualifier-type` | correct class, none of the three attributes |
| `S03-metaops/reverse.t:197-200` | `X::Syntax::CannotMeta, meta`/`operator`/`reason` | raises a generic `X::Syntax::Confused` parse error |
| `S03-operators/misc.t` (tests 35-36) | `prefixes => '^^'` / `'~~'` | no `.prefixes` |
| `S06-signature/optional.t` (tests 31-32) | `type => 'is'`, `subtype => 'rw'` | no `.type` / `.subtype` |

## Why it is a separate ticket

The gaps span unrelated subsystems — radix-literal parsing, signature traits,
metaoperator diagnosis, anonymous-routine declaration checks, and the qualified
method-call MOP path. Each needs its own diagnosis raised with the right type
*and* attributes, which is a different job from the `throws-like` plumbing that
exposed them.

## Also worth doing

`$!.backtrace` is `Nil` for a compile-time diagnosis (mutsu attaches a
`Backtrace` only on the runtime path), so `Backtrace.is-runtime` answers `Nil`
rather than `False` there. It reads falsy either way, so nothing depends on it
today, but a compile-time `Backtrace` carrying `is-runtime => False` would be
the honest shape.
