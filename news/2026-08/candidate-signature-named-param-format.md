# `X::Multi::NoMatch` candidate signatures now render named params correctly

```raku
class Foo { multi method bar(:$x!) { } }
Foo.new.bar(y => [1,2,3]);
```

```
raku:  Cannot resolve caller bar(Foo:D: :y(Array)); none of these signatures matches:
           (Foo $:: :$x!, *%_)
mutsu (before this fix): (Foo $:: Any $x, *%_)
```

`format_method_candidate_signatures`'s per-param loop (`src/runtime/class.rs`)
never branched on `pd.named`: every non-slurpy, non-invocant param rendered as
`{type} $name` (a positional), even when `pd.named` was true.

## Fix

A named param now renders as `:$name` (dropping the redundant `Any` type raku
itself omits for an untyped named param) with a trailing `!` when
`pd.required` is true and none when it's optional — `:$x!` / `:$y`. A typed
named param keeps its type ahead of the colon-sigil, matching Rakudo exactly:
`Int :$y!`. Positional params are unchanged.

Purely message-cosmetic — dispatch itself already reported no-match
correctly for a missing required named arg; only the candidate-signature
listing was wrong.

Regression tests added to `t/nomatch-candidate-signature-slurpy-and-smiley.t`
(the sibling ticket's own test file), verified against real `raku`.
