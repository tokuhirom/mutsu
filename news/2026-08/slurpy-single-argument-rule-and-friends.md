# The slurpy single-argument rule, WhateverCode over hyper calls, and Seq in a sub-signature

Continuing to reduce grondilu's `Digest::RIPEMD` against `raku` (see
`todo/tickets/digest-dist-blockers.md` blocker 2, "a WhateverCode cannot bind to
a `@`-sigil parameter") turned up four more general interpreter bugs. Each was
reduced to a one-liner before being fixed; the module now runs to completion,
though its digest is still wrong (recorded in the ticket).

## 1. `map` / `grep` / `Array.new` ignored the single-argument rule

`map(&code, +values)`, `grep(&matcher, +values)` and `Array.new(|c)` all slurp
with a `+@`-shaped parameter, and `+@` applies Raku's **single-argument rule**:
exactly one list argument is flattened into its elements, but two or more are
each one element of their own. mutsu flattened every argument unconditionally,
so

```raku
map -> [$a, $b] { "$a/$b" }, (1, 2), (3, 4)
```

ran the block four times over four Ints — dying at the destructure — instead of
twice over two pairs. That is exactly the shape `Digest::RIPEMD` builds its two
round tables with, and the original blocker's confusing symptom (a
`WhateverCode` reaching an `@`-sigil parameter) was just the first element of the
first tuple arriving where the whole tuple should have.

`Array.new` needed the same rule but not the `**@` one: `Array.new((1,2))` is two
elements, while `List.new((1,2))` — a `**@` slurpy — stays one, and
`Array.new(@a, 3)` keeps `@a` whole. An itemized `$(...)`/`$[...]` argument is
never unwrapped.

Pinned by `t/slurpy-single-argument-rule.t`.

## 2. A hyper method call did not curry a `Whatever`

`*.comb` is a `WhateverCode`; `*.comb».uc` was not. `Expr::HyperMethodCall` was
missing from the currying machinery — `contains_whatever`, `count_whatever`,
`replace_whatever_numbered`, `replace_whatever_single`, `rename_var` and the
topic/`xx` probes all knew `MethodCall` and `DynamicMethodCall` but not their
hyper twins. So the parser never wrapped the expression and the `*` was evaluated
eagerly, leaving a `List` where a closure belonged:

```raku
my $g = *.comb».uc;
$g("abc");   # No such method 'CALL-ME' for invocant of type 'List'
```

`Digest::RIPEMD` builds each round's constants with
`given *.comb».parse-base(16)`, which died on exactly this.

Pinned by `t/whatevercode-hyper-method.t`.

## 3. An `@` parameter in a sub-signature rejected `Seq` and `Range`

A destructuring `-> [@a, $b] {...}` accepted only an `Array` at `@a`, so a `Seq`
or `Range` element failed to bind — even though a plain `sub f(@a)` takes both.
Rakudo listifies a non-`Positional` `Iterable` on the way in, which is why
`@a.^name` is `List` for a `Seq` and stays `Range` for a `Range`; mutsu now does
the same, so the bound value also survives being iterated twice. A `Hash`, `Str`
or `Int` element is still a binding error.

`Digest::RIPEMD`'s `-> [&f, $r, @K, $s] {...}` binds a `Seq` at `@K`.

Pinned by `t/sub-signature-array-param-iterable.t`.

## Known remaining divergence

`Nil` still binds to an `@` sub-parameter in mutsu where Rakudo raises
`X::TypeCheck::Binding::Parameter`. Left alone here because the optional-parameter
path leans on it; noted rather than changed blind.
