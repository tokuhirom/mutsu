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
p "for literal topic",{ for 1,2 { $_ = 5 } };        # raku: X::AdHoc, Cannot assign to an immutable value
p "for keys topic",   { my %h=a=>1; for %h.keys { $_ = 5 } };   # raku: X::AdHoc
p "map literal topic",{ (1,2).map({ $_ = 5 }).eager };          # raku: X::AdHoc
p "grep topic",       { (1,2).grep({ $_ = 5 }).eager };         # raku: X::AdHoc
p "block arg topic",  { my $s = { $_ = 5 }; $s(7) };            # raku: X::AdHoc
p "pointy param",     { my $b = -> $v { $v = 1 }; $b(3) };      # raku: X::AdHoc, readonly variable
p "sub-signature",    { sub f($ (\a, \b)) { a = 1 }; f((1,2)) };# raku: X::Assignment::RO
p "sigilless incr",   { my \G = 5; G++ };            # raku: X::Multi::NoMatch
p "bind list assign", { my $x := (1,2,3); $x = 5 };  # raku: X::AdHoc, Cannot assign to an immutable value
p "bound array elem", { my @a := (1,2,3); @a.push(4) };  # raku: X::Immutable, Cannot call 'push' on an immutable 'List'
```

The topic cases share one cause: mutsu only marks `$_` readonly for *some*
immutable sources. `for 1..2 { $_ = 5 }` and `given 5 { $_ = 6 }` are correctly
rejected, but a bare comma list, `%h.keys`, `map`/`grep` blocks and a plain block
invocation are not — see the `topic_readonly` computation in
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
