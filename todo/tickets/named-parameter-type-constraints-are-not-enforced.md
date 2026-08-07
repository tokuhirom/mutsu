# A named parameter's type constraint is never checked

A type constraint on a **named** parameter is accepted and then ignored, for both
`sub` and block forms:

```raku
sub h(Int :$limit) { $limit }
say h(limit => "nope").raku;      # raku: dies    mutsu: "nope"

my &b = -> Int :$limit { $limit };
say b(limit => "nope").raku;      # raku: dies    mutsu: "nope"
```

raku dies with `Type check failed in binding to parameter '$limit'; expected Int
but got Str ("nope")`. Positional parameters *are* checked, so the gap is
specific to the named-binding path.

## Why it matters

Beyond the missing error itself, an unchecked named parameter cannot produce the
`X::TypeCheck::Binding::Parameter` that recovery code keys off. `Cro::HTTP::Router`
answers **400 Bad Request** for a failed *named* unpack and **401** for a failed
auth parameter by asking the raised exception's `.parameter.named`
(`lib/Cro/HTTP/Router.rakumod`, the `@*BIND-FAILS` loop) — with no exception
raised, the 400 branch is unreachable and such a request falls through to 404.
The 401 half of that logic works as of
`news/2026-08/binding-failures-carry-a-parameter-object.md`.

## Where to look

The positional path raises from `runtime/types/binding_signature.rs` (the
`resolved_constraint` check that produces `Constraint type check failed …` /
`Type check failed for …`). The named path binds arguments without reaching it.
A fix should route named binding through the same constraint check *and* attach
the `Parameter` object via `RuntimeError::with_parameter_object`, so
`.parameter.named` is True there.

A regression test is ready to extend: `t/typecheck-binding-parameter-object.t`
had two named-parameter assertions removed for exactly this gap.
