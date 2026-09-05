# Immutable lvalues mutsu still lets you assign to (survey)

Found by the exception-taxonomy survey in
`news/2026-08/readonly-assign-exception-taxonomy.md`. That work fixed *which*
exception a rejected assignment throws; this ticket collects the cases where
mutsu does not reject the assignment at all, which the same survey surfaced.
Every row below was re-measured against `raku` v2026.06 on **2026-09-05**, on
top of the closure-topic fix (`news/2026-09/closure-and-map-grep-topic-readonly.md`).

## Status (2026-09-05)

Closed since the survey opened:

- the element-store and `:=`-bind halves —
  `news/2026-09/immutable-element-store-and-bind.md`;
- the closure/map/grep topic family (`(1,2).map({$_=5})`,
  `(1,2).grep({$_=5})`, `{ $_ = 5 }(7)`, plus `(1..3).map`, `%h.keys.map`, the
  listop `map { $_ = 5 }, 1, 2` and its `grep` twin, `{ $_ = 5 }(3+4)` and
  `my &s = { $_ = 5 }; s(7)`) —
  `news/2026-09/closure-and-map-grep-topic-readonly.md`, pinned by
  `t/closure-topic-readonly.t`.

**Read the "how the surviving rows differ" section below before designing
anything**: two successive stated blockers for the closure-topic rows (first
"separate the two `call_compiled_closure_with_topic` callers", then ADR-0036 /
ADR-0040) were both measured to be wrong, and the runtime rule that looks
obviously right for the rows below was measured to *break* five shapes rakudo
accepts.

## The surviving rows

### A. `.map`/`.grep`/`.first` over a receiver mutsu cannot prove bare

```
my @a := (1,2,3); @a.map({$_=5}).eager      raku: X::AdHoc   mutsu: (5 5 5)
my $s = (1,2,3).Seq; $s.map({$_=5}).eager   raku: X::AdHoc   mutsu: (5 5 5)
(1,2).first({$_=5})                         raku: X::AdHoc   mutsu: 1
my %h = a=>1; %h.map({$_=9}).eager          raku: X::AdHoc   mutsu: (9)
my @a = 1,2; (@a,).map({$_=5}).eager        raku: X::AdHoc   mutsu: (5)
for %h { $_ = 5 }                           raku: X::AdHoc   mutsu: silently OK
```

The shipped fix keys off `Compiler::for_iterable_yields_bare_items` applied to
the *receiver expression*, which deliberately answers `false` for a variable and
for any derived receiver. These rows are exactly the receivers it cannot prove:
an `@`-variable `:=`-bound to a `List`, a `$`-variable holding a `Seq`, a
`%`-variable (whose iteration mints fresh `Pair`s), and a one-element list
literal built from an array variable. `.first` additionally never reaches the
marking at all — only the two map loops and the grep loop consult
`CompiledCode::immutable_topic`.

`for %h` is the same row from the `for` side. Extending
`for_iterable_yields_bare_items` to `Expr::HashVar` looks like the one-line fix
for both, but the `for` loop pairs its topic mark with the
`__mutsu_deep_readonly::_` env flag, which would then also reject
`for %h { .value = 5 }` — a write rakudo performs. Whatever closes this row has
to separate "the topic itself is immutable" from "everything reachable through
it is".

### B. Element and argument shapes where mutsu drops the write instead

Not immutability rows at all — rakudo performs these writes and mutsu silently
loses them, which is the *opposite* failure and must not be "fixed" by teaching
the marking to reject them:

```
my @a=1,2,3; @a.list.map({$_=7}).eager; @a      raku [7 7 7]   mutsu [1 2 3]
my @a=1,2,3; @a[0..1].map({$_=5}).eager; @a     raku [5 5 3]   mutsu [1 2 3]
my $x=[1,2,3]; $x.map({$_=5}).eager; $x         raku [5 5 5]   mutsu [1 2 3]
my @a=1,2,3; @a.first({$_=5}); @a               raku [5 2 3]   mutsu [1 2 3]
my $v=1; my $b={$_=9}; $b($v); $v               raku 9         mutsu 1
my @a=1,2,3; my $b={$_=9}; $b(@a[0]); @a        raku [9 2 3]   mutsu [1 2 3]
my @a=1,2,3; for @a[0..1] { $_ = 5 }; @a        raku [5 5 3]   mutsu [1 2 3]
```

These are the real ADR-0036/ADR-0040 surface: the element handed to the topic is
a bare value, not a cell, so the write has nowhere to land. Note that mutsu
*does* hand out cells for `@a.values` and `%h.values` (those rows write through
correctly today), so the gap is per-producer, not universal.

### How the surviving rows differ from what was fixed (measured, do not skip)

The tempting rule for section A is the runtime one the lazy `for` path already
uses (`vm_for_loop_lazy.rs`): mark the topic read-only when
`!item.is_container_ref()`. It was implemented and measured on 2026-09-05, and it
converts **every row in section B** from a silently-dropped write into a spurious
throw, because a real `Array`'s elements are stored bare. A source-*kind* rule
(`ArrayKind::List`/`ItemList` ⇒ immutable) fails the same way: `@a.list` and
`@a[0..1]` both produce a `List` whose elements rakudo still writes through, and
`@a.list.grep({$_=5})` currently writes back correctly via
`overwrite_array_bindings_by_identity`, so a kind-based refusal would regress a
row that works.

So section A cannot be closed by a local runtime test. Either the receiver
oracle grows (a compile-time notion of "this variable is `:=`-bound to an
immutable Positional" / "this variable holds a `Seq`", which the compiler already
tracks partially in `scalar_bind_*`), or section B is closed first — once an
element really is a cell, `is_container_ref()` becomes a sound oracle for both.
Closing B first is the architecturally cleaner order.

### C. A `$` bind of a MUTABLE container is still assignable

rakudo's rule is sharper than "immutable": `$x = v` needs `$x` bound to a
**Scalar** container, and no other container qualifies — a real `Array`, a
`Hash`, a `Map` and a `Pair` all refuse it, though each is mutable through its
own interface.

```
my @a = 1,2,3; my $x := @a;        $x = 5     # raku: X::AdHoc; mutsu: OK, @a becomes 5
my $x := [1,2,3];                  $x = 5     # raku: X::AdHoc; mutsu: OK
my @a := (1,2,3); my $x := @a;     $x = 5     # raku: X::AdHoc; mutsu: OK
my $x := {a=>1};                   $x = 5     # raku: X::AdHoc; mutsu: OK
my $x := Map.new((a=>1));          $x = 5     # raku: X::AdHoc; mutsu: OK
my $x := (a => 1);                 $x = 5     # raku: X::AdHoc; mutsu: OK
```

Deliberately left out of the 2026-09-05 element-store fix, which extended
`bind_source_has_no_container`'s allowlist only to immutable Positionals. Two of
these rows (`my $x := @a`, `my $x := %h`) do not even reach that decision — a
bind whose RHS is a simple variable carries a NAMED source and is excluded from
the marking outright — so closing this family means deciding what a named
`@`/`%` source should imply for a `$` target, not just widening a match arm. The
`$x.push(...)` aliasing those binds exist for must keep working; only the
whole-value `=` is refused.

One near-miss in the same family: `my $x := $(1,2,3); $x = 5` throws `X::AdHoc`
in both, but rakudo words it "Cannot assign to a readonly variable or a value"
where mutsu says "Cannot assign to an immutable value".

### D. A `gather` sequence's element store

```
my $s = (gather { take 1; take 2 }); $s[0] = 5
    # raku: X::Assignment::RO, "Cannot modify an immutable Int (1)"
    # mutsu: silently succeeds
```

The `.Seq` twin was fixed by teaching `try_seq_element_cell_assign` to refuse a
materialized non-container element. A `gather` result is a `ValueView::LazyList`
in mutsu, not a `Seq`, and it shares that representation with the lazy `@`-array
whose element assignment is *legitimate* (`my @a = 1,2,4...Inf; @a[2] = 99` is
real raku, and `restore_lazy_array_slot` exists to support it). So the refusal
cannot simply be extended to `LazyList`: it needs the `array_context` /
`list_context` distinction to be the oracle, which is separate work.

### E. An associative subscript of a `Seq`

```
my $s = (1,2,3).Seq; $s<a> = 5
    # raku: X::AdHoc, "Type Seq does not support associative indexing."
    # mutsu: silently succeeds
```

rakudo refuses the *subscript*, not the store, so this is not an immutability row
at all — it belongs with whatever enforces the Positional/Associative protocol
per type.

### F. An inline declaration inside a list literal

```
my $a = 1; (my $x = $a, 6)[0] = 10
    # raku:  x=10 a=1  (an inline declaration in a list literal denotes the
    #        freshly-declared variable's container, so the store writes it)
    # mutsu: X::Assignment::RO, "Cannot modify an immutable List ((1 6))"
```

Extending `scalar_container_alias_name` to cover `Expr::DoStmt(VarDecl)` was
tried and did not reach it, so the inline declaration does not arrive in that
shape at this position; finding what it *does* arrive as is the next step.

## Messages that are close but not exact

These already throw the right class; only the rendered value differs:

- a **compound** assignment to an immutable topic. rakudo answers
  `X::Assignment::RO` naming the element ("Cannot modify an immutable Str (a)")
  for `$_ .= uc` and `$_ ~= "!"`, and `X::AdHoc` for `$_ += 1`. mutsu answers
  `X::AdHoc` for `.=` and `X::Multi::NoMatch` "Cannot resolve caller
  postfix:<++>(_)" for `+=`/`~=` — the latter because the recompiled map/grep
  block body routes `+= 1` through the increment check, which hardcodes
  `postfix:<++>` as the operator name. All three now correctly *die*
  (`t/closure-topic-readonly.t` pins that much); only the class/wording differs.
- `my constant @A = 1,2,3; @A = 5` — raku names the *element* ("Cannot modify an
  immutable Int (1)", because a list assignment writes into the immutable List's
  elements); mutsu names the container ("Cannot modify an immutable List
  ((1 2 3))"). Same for `my @a is List`.
- `my constant %C = (a=>1); %C = (b=>2)` — raku "Cannot modify an immutable Pair
  (a => 1)"; mutsu renders the pair with a tab instead of `=>`.
- `my %m := mix <a b>; %m = (c=>1)` — raku "immutable Mix (Mix(a b))", mutsu
  "immutable Mix (a b)".
- `sub g() {...}; g() = 5` — raku "Cannot modify an immutable Int (42)", mutsu
  "sub 'g' is not rw"; `$obj.x = 5` on a non-`rw` attribute — raku "Cannot modify
  an immutable Int (1)", mutsu "method 'x' is not rw".
- `my @a := (1,2,3); @a.splice(0,1)` — raku does not define a `splice` candidate
  on a plain `List` (`X::Multi::NoMatch`, "Routine does not have any
  candidates"); mutsu reports `X::Immutable` "Cannot call 'splice' on an
  immutable 'List'" (the same message the other five list mutators use, since
  `splice` shares their dispatch check).

## Corrected blocker attributions (do not re-derive these)

- **ADR-0040 (store-side element itemization) is not the blocker.** Slices 1 and
  2 landed and moved no row. Itemization is not container-ness: it makes an
  element *render* as one item, not become a cell, and `is_container_ref()` stays
  false for an ordinary itemized element.
- **ADR-0036 (element containers from subscripts/pairs) is not the blocker
  either.** Slice 4 completed it and moved no row. ADR-0036 is about what a
  *pair producer* hands out; these rows are about what the *subscript store path*
  and the *topic binding* accept.
- **"Separate the two `call_compiled_closure_with_topic` callers" was not a
  blocker.** Measured 2026-09-05: `capture_rw_topic == true` has exactly one
  producer in the tree, so the separation already existed, and the two
  `.map`/`.grep` rows never reached that function at all.
