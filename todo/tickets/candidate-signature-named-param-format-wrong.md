# X::Multi::NoMatch candidate signatures render a named param as positional

Found while fixing `nomatch-candidate-signature-slurpy-and-smiley.md`
(`format_method_candidate_signatures`, `src/runtime/class.rs`).

```raku
class Foo {
    multi method bar(:$x!) { }
}
Foo.new.bar(y => [1,2,3]);
```

```
raku:  Cannot resolve caller bar(Foo:D: :y(Array)); none of these signatures matches:
           (Foo $:: :$x!, *%_)
mutsu: Cannot resolve caller bar(Foo:D: :y(Array)); none of these signatures matches:
           (Foo $:: Any $x, *%_)
```

`format_method_candidate_signatures`'s per-param loop (`src/runtime/class.rs:344-378`)
never branches on `pd.named`: every non-slurpy, non-invocant param renders as
`{type} ${name}` (a positional), even when `pd.named` is true. A genuinely named
param should render as `:${name}` (dropping the redundant `Any` type — raku omits
an untyped named param's type entirely) with a trailing `!` when `pd.required` is
true (`:$x!`) and no `!` when it has a default/is optional (`:$x`). A *typed*
named param would need its type kept (e.g. `:Int $y!` — unverified, not checked
against raku for this ticket).

Scope note: purely message-cosmetic, same as the two bugs this file's sibling
ticket fixed — dispatch itself already reports no-match correctly for a missing
required named arg; only the candidate-signature listing is wrong.
