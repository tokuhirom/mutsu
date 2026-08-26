# Immutable lvalues mutsu still lets you assign to (survey)

Found by the exception-taxonomy survey in
`news/2026-08/readonly-assign-exception-taxonomy.md`. That work fixed *which*
exception a rejected assignment throws; this ticket collects the cases where
mutsu does not reject the assignment at all, which the same survey surfaced.
Every row was probed against `raku` v2026.06.

## Cases (raku throws, mutsu silently succeeds)

```raku
sub p($l, &c) { my $r = try { c() }; say $l, " => ", ($! ?? $!.^name ~ " | " ~ $!.Str !! "OK") }

p "list elem",        { (1,2,3)[0] = 9 };            # raku: X::Assignment::RO, Cannot modify an immutable List ((1 2 3))
p "range elem",       { (1..3)[0] = 9 };             # raku: X::Assignment::RO, Cannot modify an immutable Range (1..3)
p "Seq elem",         { my $s = (1,2,3).Seq; $s[0] = 5 };   # raku: X::Assignment::RO, immutable Int (1)
p "bound list elem",  { my @a := (1,2,3); @a[0] = 9 };      # raku: X::Assignment::RO, immutable List
p "assign to Nil",    { Nil = 5 };                   # raku: X::Assignment::RO, Cannot modify an immutable Nil value
p "assign to type",   { Int = 5 };                   # raku: X::Assignment::RO, Cannot modify an immutable 'Int' type object
p "assign to enum",   { enum Fo <A B>; A = 3 };      # raku: X::Assignment::RO, Cannot modify an immutable Fo (A)
p "map literal topic",{ (1,2).map({ $_ = 5 }).eager };          # raku: X::AdHoc
p "grep topic",       { (1,2).grep({ $_ = 5 }).eager };         # raku: X::AdHoc
p "block arg topic",  { my $s = { $_ = 5 }; $s(7) };            # raku: X::AdHoc
p "pointy param",     { my $b = -> $v { $v = 1 }; $b(3) };      # raku: X::AdHoc, readonly variable
p "sub-signature",    { sub f($ (\a, \b)) { a = 1 }; f((1,2)) };# raku: X::Assignment::RO
p "sigilless incr",   { my \G = 5; G++ };            # raku: X::Multi::NoMatch
p "bind list assign", { my $x := (1,2,3); $x = 5 };  # raku: X::AdHoc, Cannot assign to an immutable value
p "bound array elem", { my @a := (1,2,3); @a.push(4) };  # raku: X::Immutable, Cannot call 'push' on an immutable 'List'
```

## Status update (2026-08-26)

Four topic rows are now **fixed** and moved out of this list:
`for 1,2`, `for (1,2)`, `for <a b>` and `for %h.keys` throw `X::AdHoc` "Cannot
assign to an immutable value", matching raku
(`news/2026-08/topic-var-name-still-scalar-for-literal-alias.md`). The mechanism
is `ForLoopSpec::source_items_are_bare`, a *provable* compile-time property
(`Compiler::for_iterable_yields_bare_items`).

The remaining topic rows — `map`/`grep` block topics, a plain block invocation
(`my $s = { $_ = 5 }; $s(7)`), a pointy block parameter (`-> $v { $v = 1 }`) and
`for %h` (whose items are immutable `Pair`s) — could **not** be closed the same
way, and the measurement explains why. Raku's rule is per item: the topic is
writable exactly when the item is a container.

```
for @a         Scalar      for 1,2        Int
for @a.values  Scalar      for (1,2)      Int
for $a, $b     Scalar      for <a b>      Str
for @a[0..1]   Scalar      for %h.keys    Str
for @a.map({}) Scalar      for %h         Pair
```

mutsu cannot evaluate that at runtime because real `Array`/`Hash` elements are
stored **bare** — see `todo/deep/element-itemization-lost-in-scalar-binding.md`
and ADR-0040. `vm_for_loop_lazy.rs` already applies the correct runtime test
(`item.is_container_ref()`), which is why `for gather { … }` is rejected
correctly; applying the same test on the eager path would additionally mark
`for @a[0..1]` and `for @a.map(…)` read-only, inventing throws raku does not
have. **These rows are therefore blocked on ADR-0040's store-side element
itemization, not on the topic-marking code.**

The `map`/`grep`/block-argument rows are a *different* blocker: those topics are
bound by the block invocation path, not the loop, and none of those paths marks
the bound topic at all. `sub f($x) { $x = 1 }` is correctly rejected, but the
pointy-block form `-> $v { $v = 1 }` is not — named routines mark their params
(`vm_call_light.rs`), the closure-call path does not.

The original note, for reference: mutsu only marks `$_` readonly for *some*
immutable sources — see the `topic_readonly` computation in
`src/vm/vm_for_loop_body.rs` and the corresponding sites in
`vm_for_loop_intrange.rs` / `vm_for_loop_lazy.rs` / `vm_given_when_ops.rs`.

The `my $x := (1,2,3)` case is the deliberate conservatism in
`src/vm/vm_var_assign_set_local.rs`: the `:=`-to-literal readonly marking fires
only for scalar-shaped values (`Int`/`Str`/`Num`/`Rat`/`Bool`/`Complex`), because
anything container-like or written-through (`ContainerRef`, `Proxy`,
`HashEntryRef`, `is raw`) must stay writable and an overlooked kind becomes a
hard error. Widening it needs the container/no-container distinction to be a
property of the *value*, not a whitelist of view kinds.

The element cases (`(1,2,3)[0] = 9`, `Range`, `Seq`) need the subscript store
path to know its target is an immutable container.

## Messages that are close but not exact

These already throw the right class; only the rendered value differs:

- `my constant @A = 1,2,3; @A = 5` — raku names the *element*
  ("Cannot modify an immutable Int (1)", because a list assignment writes into
  the immutable List's elements); mutsu names the container
  ("Cannot modify an immutable List (1 2 3)"). Same for `my @a is List`.
- `my constant %C = (a=>1); %C = (b=>2)` — raku "Cannot modify an immutable Pair
  (a => 1)"; mutsu renders the pair with a tab instead of `=>`.
- `my %m := mix <a b>; %m = (c=>1)` — raku "immutable Mix (Mix(a b))", mutsu
  "immutable Mix (a b)".
- `sub g() {...}; g() = 5` — raku "Cannot modify an immutable Int (42)", mutsu
  "sub 'g' is not rw"; `$obj.x = 5` on a non-`rw` attribute — raku
  "Cannot modify an immutable Int (1)", mutsu "method 'x' is not rw".
