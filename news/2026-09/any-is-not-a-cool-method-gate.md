# A `Cool` method on an undefined `Any` throws instead of answering `(Any)`

`Any` is not a `Cool` — it is the other way round — so `uc`, `chars`, `comb`,
`abs`, `substr` and the rest of that surface simply do not exist on an
undefined invocant. raku says so:

```
my $x; say $x.comb(/\w+/);   # No such method 'comb' for invocant of type 'Any'
```

mutsu answered `(Any)`.

The mechanism was mutsu's by-name native dispatch. Its cascades recognize the
method *name*, stringify the receiver and answer out of that string; a type
object stringifies to its gist, so `$x.comb(/\w+/)` combed the literal text
`"(Any)"` and returned `("Any",)`. `.uc` answered `"(ANY)"`, `.chars` answered
`5`, `.bytes` answered `5`, `.IO` handed back an `IO::Path` for a file named
`(Any)`.

That is worse than a missing method, and the documentation example that turned
it up (`raku-doc/doc/Language/objects.rakudoc:48`, plus a cluster of seven rows
in `perl-nutshell.rakudoc`) shows why — the wrong value does not stop, it
travels:

```raku
my $formatted-text;
my @words = "Abe", "Lincoln";
@words.push("said", $formatted-text.comb(/\w+/));
say @words;                      # raku: dies.  mutsu: [Abe Lincoln said (Any)]
```

## The fix

This is [ADR-0051](../../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md)'s
P4 rule — "an unresolved `Cool`-only method throws rather than stringifying the
receiver" — applied to the one receiver shape its gates never saw. Those gates
sit inside a `ValueView::Instance` arm, so the `Any`/`Mu` **type object** walked
straight past them into the cascades. `src/runtime/any_cool_method_gate.rs` is
the missing arm, wired into both dispatch entries: `call_method_with_values`
(the interpreter) and `try_native_method` (the opcode path, which reaches the
cascades without passing through the first).

The gated name set is the union of two independently raku-verified sources, and
neither subsumes the other: ADR-0051's hand-curated `cool_only_builtin_method`
list carries names with no `Cool` row of their own (`bytes`, `lazy`, `race`,
`Date`), and the generated row catalog carries rows the list predates
(`printf`, `is-prime`, the native-int coercion family). `Any.^can` — rakudo's
and mutsu's alike — is empty for every name in either. That last point is the
interesting one: mutsu's introspection already knew `Any` has no `comb`, and
only dispatch disagreed.

`match` and `split` are deliberately excluded. raku declares both on `Any`;
their candidates merely refuse an undefined invocant, so raku answers
`X::Multi::NoMatch` / a use-of-uninitialized warning for them, never
`X::Method::NotFound`. Gating them would swap one wrong answer for another.

## Verification

`t/any-cool-method-not-found.t` pins the repro, the doc example (including that
nothing reaches the array), nine more `Cool` names, the `Mu` twin, an unknown
name as the control that the ordinary `X::Method::NotFound` path is unchanged,
and `Any`'s own surface (`.defined`, `.gist`, `.raku`, `.^name`, `.WHAT`,
`.WHICH`, `.elems`) plus a defined `Str`/`Int` receiver. All 26 assertions pass
under `raku` as well as mutsu.

A 184-name sweep of `my $x; $x.<name>` against rakudo v2026.07 confirms the
change introduces no new divergence: the seventeen names that still disagree are
all pre-existing gaps of a different kind (a zero-argument call of a method that
needs arguments, where raku raises `X::Multi::NoMatch`), and none of them is in
the gated set.

Closes #7773.
