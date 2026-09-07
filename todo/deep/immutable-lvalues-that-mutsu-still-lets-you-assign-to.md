# Immutable lvalues mutsu still lets you assign to (survey)

Found by the exception-taxonomy survey in
`news/2026-08/readonly-assign-exception-taxonomy.md`. That work fixed *which*
exception a rejected assignment throws; this ticket collects the cases where
mutsu does not reject the assignment at all, which the same survey surfaced.
Every surviving row below was re-measured against `raku` on **2026-09-07** on a
fresh build of `main` at `2c7fc1dde`; every one of them still reproduced, and
the ones this file no longer lists were closed on that same day. C2, D, E and F
were re-verified unchanged then (D's shape had drifted — see its row).

## Status (last updated 2026-09-07)

Closed since the survey opened:

- **section C in full (2026-09-06)** — a `$` bound to a non-Scalar container now
  refuses whole-value assignment:
  `news/2026-09/scalar-bind-to-a-non-scalar-container-refuses-assignment.md`;
- the element-store and `:=`-bind halves —
  `news/2026-09/immutable-element-store-and-bind.md`;
- the closure/map/grep topic family (`(1,2).map({$_=5})`,
  `(1,2).grep({$_=5})`, `{ $_ = 5 }(7)`, plus `(1..3).map`, `%h.keys.map`, the
  listop `map { $_ = 5 }, 1, 2` and its `grep` twin, `{ $_ = 5 }(3+4)` and
  `my &s = { $_ = 5 }; s(7)`) —
  `news/2026-09/closure-and-map-grep-topic-readonly.md`, pinned by
  `t/closure-topic-readonly.t`.

Tracked separately, same family, different producer: a **raw-invocant method**
called on an immutable `List` element (`my $l = (1,2); $l[0].mut`) succeeds
silently where raku dies —
`todo/tickets/immutable-list-element-write-is-silently-dropped.md` (2026-09-06).
Every *ordinary* store to such an element is already refused correctly, so that
one is a method-call-path gap rather than a store-path gap, and it belongs to
ADR-0067's L4/L5/M1/M2 readonly-enforcement rows.

Section B was re-measured on **2026-09-07** and three of its four producers
were closed the same day
(`news/2026-09/slice-first-and-block-topic-element-containers.md`); only
producer 1 (`.list` feeding `map`) survives. Section C was re-measured on
2026-09-06 (all seven rows diverged) and then closed.

**Section A was re-measured and mostly closed on 2026-09-07**
(`news/2026-09/immutable-topic-receiver-oracle-widened.md`, pinned by
`t/immutable-topic-receiver-oracle.t`): the compile-time receiver oracle grew,
which is the second of the two routes the "how the surviving rows differ"
section below names. What is left of A is the two receivers a *syntactic* oracle
genuinely cannot decide, plus two rows found while measuring. Sections A(rest),
B(1), C2, D, E and F are what remain.

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
my @a := (1,2,3); @a.grep({$_=5}).eager     raku: X::AdHoc   mutsu: (5 5 5)
my @a := (1,2,3); for @a { $_ = 5 }         raku: X::AdHoc   mutsu: silently OK
my $s = (1,2,3).Seq; $s.map({$_=5}).eager   raku: X::AdHoc   mutsu: (5 5 5)
my $s = (1,2,3).Seq; $s.grep({$_=5}).eager  raku: X::AdHoc   mutsu: (5 5 5)
```

`Compiler::for_iterable_yields_bare_items` is a verdict on the receiver's
*syntax*, and these two receivers are plain variables: an `@`-variable
`:=`-bound to a `List`, and a `$`-variable holding a `Seq`. Nothing in the
expression says so. Closing them needs either a compile-time notion of "this
variable is `:=`-bound to an immutable Positional" / "this variable holds a
`Seq`" (the compiler already tracks something adjacent in `scalar_bind_*`), or
section B closed first so `is_container_ref()` becomes a sound runtime oracle.

Four rows that used to sit here — `(1,2).first({$_=5})`, `%h.map({$_=9})`,
`(@a,).map({$_=5})` and `for %h { $_ = 5 }` — were closed on 2026-09-07 by
widening that same oracle (a `%` variable, `.List`/`.pairs`/`.antipairs`/`.kv`
on an `@`/`%` variable, and a list literal with any provably-bare item), by
adding `"first"` to `method_binds_immutable_topic`, and by splitting the `for`
loop's shallow topic mark from its `__mutsu_deep_readonly::_` flag — which is
exactly the separation this section predicted was required, and it was.

#### Two more rows in this family, both located but not fixed

```
my $v=1; my $b={$^x=9}; $b($v); $v
    # raku: X::Assignment "Cannot assign to a readonly variable or a value"
    # mutsu: silently assigns the placeholder local, $v stays 1
my $c = class { has $.n = 1 }.new; my $b={$_=9}; $b($c.n); $c.n
    # raku: X::AdHoc "Cannot assign to an immutable value"
    # mutsu: silently succeeds, $c.n stays 1
```

The first is located (2026-09-07): a `$`-sigiled **pointy** parameter is marked
readonly at the call site via `CompiledCode::pointy_alias_param`, set in
`Compiler::compile_expr_lambda`. A **placeholder** block (`{ $^x = 9 }`) is
compiled by `compile_expr_anon_sub` instead, which emits `MakeAnonSub` and never
sets that flag, so `sub f($x) { $x = 5 }` and `-> $x { $x = 5 }` both die
correctly while `{ $^x = 5 }` does not. Read the long comment above
`pointy_alias_param` before moving the marking: injecting a `MarkReadonly`
prologue into the body instead leaks the mark permanently through the fast
native map/grep/first loops, and that reached CI once already.

### B. Element and argument shapes where mutsu drops the write instead

Not immutability rows at all — rakudo performs these writes and mutsu silently
loses them, which is the *opposite* failure and must not be "fixed" by teaching
the marking to reject them.

Re-measured 2026-09-07 on a fresh build; **producers 2, 3 and 4 are CLOSED**
(`news/2026-09/slice-first-and-block-topic-element-containers.md`, pinned by
`t/slice-hands-out-element-containers.t`,
`t/first-scans-element-containers.t` and `t/block-call-binds-topic-raw.t`).
What remains is producer 1:

```
my @a=1,2,3; @a.list.map({$_=7}).eager; @a      raku [7 7 7]   mutsu [1 2 3]
my $x=[1,2,3]; $x.map({$_=5}).eager; $x         raku [5 5 5]   mutsu [1 2 3]
```

1. **`.list` (and an itemized `$[...]`) feeding `map` specifically.** Measured
   across the cross-product:

   | receiver | `.map({$_=5})` | `.grep({$_=5})` |
   |---|---|---|
   | `@a` | writes through | writes through |
   | `@a.values` | writes through | writes through |
   | `@a.list` | **lost** | writes through |

   So `.list.map` is a ONE-path difference inside `map`, not a `.list` problem:
   three of the four cells already work, and `grep` handles the same receiver
   correctly.

   **Located 2026-09-07** (breakpoint counts on the three candidate paths, per
   probe):

   - `@a.map({$_=5})` is served by **`try_native_array_map`**
     (`vm/vm_native_map.rs`), which does the rw writeback itself. Its receiver
     gate is `ValueView::Array(items, ArrayKind::Array)` — a *plain concrete*
     array only.
   - `@a.list` is `ArrayKind::List`, so that gate **declines** (deliberately —
     the comment there says `List` "has its own one-arg-rule / Seq-returning
     semantics"), and the call falls through to `dispatch_map_method`.
   - Since ADR-0058 step 2, `dispatch_map_method` returns
     `Value::seq_deferred(SeqSource::MapGrep { items: Arc<Vec<Value>>, .. })`,
     and the pull arm (`vm/vm_helpers_lazy.rs`) runs **`eval_map_over_items`**,
     the NON-rw evaluator, over that flat snapshot. There is no source node left
     to write back to, so the write has nowhere to land.
   - `.list.grep` works because grep does not go through any of that: it
     promotes the matched elements to `ContainerRef` cells and writes the
     promoted array back with `overwrite_array_bindings_by_identity`
     (`runtime/methods_collection_ops/grep.rs`), which is **node-based** rather
     than kind-gated.

   The fix has two candidate shapes, and both touch ADR-0058 machinery — read
   ADR-0058 §8/§9 and
   `todo/deep/deferred-map-callback-runs-in-the-consuming-frames-env.md` first,
   because another change is in flight on exactly this path:

   - carry the source's backing node (not just an `Arc<Vec<Value>>` snapshot)
     into `SeqSource::MapGrep`, use `eval_map_over_items_rw` at pull time, and
     call `overwrite_array_bindings_by_identity` when it reports `wrote_back`; or
   - adopt grep's shape at `.map` time: promote the source elements to cells and
     write the promoted array back by identity before deferring. Structural
     promotion runs no user code, so it is compatible with the deferral — but
     grep promotes only the *matched* indices, and promoting every `.map`
     source's elements is a decision about what an element is, not a local
     change. (Note the promotion primitives now exist and are shared:
     `Interpreter::array_element_cells` / `array_element_cell_at` /
     `hash_element_cell_at`, `vm/vm_element_producers.rs`.)

2. **A SLICE hands out bare values.** The **associative** half is CLOSED
   (2026-09-07): `%h<a b>` promotes its elements and `for %h<a b> { $_ = 5 }`
   writes through. The **positional** half is **PARKED**, and its five rows in
   `t/slice-hands-out-element-containers.t` are `todo`-marked.

   The diagnosis is right and was verified: raku's slice IS the containers
   (`@a[0..1]` is the two `Scalar`s), so the gap is the *producer*, not the
   writeback — the file's original proposal to carry the source name and the
   index list to `vm_loop_writeback.rs` would not have covered
   `@a[0..1].map(...)`, `%h<a b>`, or a slice element passed as an argument.
   A `slice_array_entry` that promotes each in-bounds element of a mutable
   Array on the read path (`vm/vm_var_index_ops.rs`) closes every row.

   **What blocks it**: that promotion corrupts `Text::CSV`. Its whitelisted
   `t/90_csv.t` test 503 ("AOA parse out") fails because
   `csv(in => $aoa.iterator, out => $fno)` leaves every row of `$aoa` as
   `IterationEnd` — the source array's own slots are written through by the
   consuming loop (`gather while $in.pull-one () -> \r { ... }` in
   `Text/CSV.rakumod`). Measured on a clean run against a `main` baseline that
   passes the file; the associative half, `.first` and the block-call topic are
   all innocent (each was disabled in turn, alone).

   Two exclusions a future producer must inherit, both found by the local suite:
   a HOLE that reads as the container's `is default(...)` value is not promoted
   (`@a[3]:delete` then `@a[2,3,4]` must read the default, not the marker), and
   the list-destructuring staging temp is excluded from the shared gate
   entirely — see
   `todo/deep/containerref-holding-a-hash-is-indistinguishable-from-itemization.md`
   for why the alternative (decontainerizing in the hash initializer) is blocked
   by a representation ambiguity. A third is now required: whatever the
   `Text::CSV` shape needs, which has to be understood before the positional
   half can land.

3. ~~**A block called with an argument does not alias `$_` to it.**~~ **CLOSED
   2026-09-07**, in two halves. A plain scalar argument goes through
   `Interpreter::pending_call_topic_source`, the exact sibling of
   `pending_call_topic_bare` (same producer, same one-call lifecycle), and binds
   the topic through the shared-cell recipe `binding_signature.rs` already uses
   for an `is rw` parameter. A subscript argument needed the missing producer
   below.

   Gated on `CompiledCode::writes_topic` (set in `compute_free_vars` when the
   body writes `_` by name), because the alias is not free: it installs a cell
   in the CALLER's env under the argument's source name and registers an exit
   writeback. Firing it for every block call broke `Log::Timeline` — its nested
   `Task.log: { ... }` blocks never touch `$_`, and the writeback re-published
   an outer task's state over the inner one's, so `logging.rakutest` reported
   the OUTER task's id for the inner task's end entry.

4. ~~**`.first`** never reaches the topic marking at all.~~ **CLOSED
   2026-09-07 — and the stated diagnosis was wrong.** "Only the two map loops and
   the grep loop consult `CompiledCode::immutable_topic`" is the *immutability*
   question (section A). Section B's `.first` row was the same producer gap:
   `@a.values.first({$_=5})` and `@a.Seq.first({$_=5})` already wrote through, so
   the topic path was fine — `@a.first(...)` simply scanned bare items.
   `vm/vm_native_first.rs` (the path that actually serves it; `dispatch_first` is
   only reached for the adverb forms) now scans the element containers and
   decontainerizes its answer.

The row that was NOT in this section but shared producer 2's missing piece —
`my $b = -> $x is rw { $x = 9 }; $b(@a[0])` dying with "expects a writable
container" — is closed with it, and so is the headline of
`todo/tickets/subscript-argument-container-producer.md`, by `OpCode::IndexArgRef`
(ADR-0067's subscript-ARGUMENT producer, the twin of `IndexInvocantRef` one
position over). That ticket's two asides remain open, plus one new row it
records.

#### A hash-sourced `map`/`grep` Pair does not share the hash's value container

Found 2026-09-07 while measuring section A, and it is the OPPOSITE direction —
mutsu throws where rakudo writes:

```
my %h = a=>1; %h.map({ .value = 9 }).eager; %h    raku {a => 9}  mutsu: throws
my %h = a=>1; %h.grep({ .value = 9 }).eager; %h   raku {a => 9}  mutsu: throws
my %h = a=>1; %h.pairs.map({ .value = 9 }); %h    raku {a => 9}  mutsu {a => 9}
my %h = a=>1; for %h { .value = 9 }; %h           raku {a => 9}  mutsu {a => 9}
```

("throws" = `X::Assignment::RO`, "Cannot modify an immutable Int (1)".)

The producer is `runtime/utils/list.rs`'s `value_to_list`, which a direct
`%h.map`/`%h.grep` receiver goes through: its `ValueView::Hash` arm builds each
item with `items.typed_pair(k, v.clone())`, and `typed_pair` **decontainerizes**
the element cell deliberately, so that a pair's value matches a `%h<k>` read and
a `.values` element (`t/bind-hash-value-pairs.t`). `%h.pairs` and the `for %h`
loop use a different producer and keep the cell. Making the map/grep-over-a-hash
path use the `.pairs` producer is the shape of the fix, but `value_to_list` is
a very widely shared funnel — measure who else depends on the decontainerizing
arm before changing it.

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
Three of section B's four producers hand out cells as of 2026-09-07, so that
second route is now most of the way there for a *slice* and for `.first`; it is
still false for a plain `@a[0]` read and for `@a`'s own elements outside a
producer, so the runtime rule is not yet sound.
Closing B first is the architecturally cleaner order.

**Update 2026-09-07:** the *first* route — growing the receiver oracle — turned
out to cover far more of section A than this paragraph assumed, and to need no
runtime test at all. The exact rule, measured against rakudo, is that a list
literal is **element-wise** (`($x, $y)` is a `List` of two `Scalar`s and writes
through; any item with no `Scalar` behind it makes the topic immutable), and
that a `%` variable and the `.List`/`.pairs`/`.antipairs`/`.kv` views mint fresh
items while `.list`/`.values`/`.Seq` hand out the source's own containers. Those
are all syntactic, so they are decidable where a `:=`-bound `@a` is not. See
`news/2026-09/immutable-topic-receiver-oracle-widened.md`; only the two
variable-receiver rows above still need route two.

### C. A `$` bind of a MUTABLE container is still assignable — **CLOSED 2026-09-06**

`news/2026-09/scalar-bind-to-a-non-scalar-container-refuses-assignment.md`,
pinned by `t/scalar-bind-to-non-scalar-container.t`.

rakudo's rule is sharper than "immutable": `$x = v` needs `$x` bound to a
**Scalar** container, and no other container qualifies — a real `Array`, a
`Hash`, a `Map` and a `Pair` all refuse it, though each is mutable through its
own interface. All seven rows now throw `X::AdHoc: Cannot assign to an immutable
value`, matching raku's class and wording, and the `my $x := @a` row no longer
silently overwrites `@a`.

The fix is `bind_source_is_non_scalar_container` beside the existing
`bind_source_has_no_container` (`vm/vm_var_assign_set_local.rs`). Two things the
file predicted correctly and one it did not:

- the two named-source rows (`my $x := @a`, `my $x := %h`) really do not reach
  the immutability test, and needed their own arm keyed on the source name's
  sigil;
- the marking has to be restricted to a **declaration**. A parameter bind reaches
  the same store, and an `is raw` / `\x` parameter bound to an array must stay
  assignable;
- `my $x := (a => 1)` arrives as `ValueView::ValuePair`, not `ValueView::Pair`,
  so matching only the latter left that row passing.

One near-miss remains: `my $x := $(1,2,3); $x = 5` throws `X::AdHoc` in both, but
rakudo words it "Cannot assign to a readonly variable or a value" where mutsu
says "Cannot assign to an immutable value".

### C2. Subscripting an existing non-container element autovivifies instead of dying

```
my @a = 1,2,3; @a[1][0] = 9
    # raku:  X::Assignment::RO, "Cannot modify an immutable Int (2)"
    # mutsu: silently succeeds, @a becomes [1 [9] 3]
```

Noticed 2026-09-06 while fixing
`news/2026-09/chained-index-autoviv-tracks-holes.md`. Subscripting an element
that already holds a plain value is an error in raku, not an autovivification —
the chained-index walk's `needs_viv` test treats "not an Array/Hash/ContainerRef"
as "vivify me", and a defined `Int` falls into that bucket. The nested handler
already has the right shape for the ROOT (`root_needs_viv` deliberately excludes
a *defined* value, with a comment saying raku dies there and that papering over
it would clobber the value); the same distinction is missing one level down.

### D. A `gather` sequence's element store

```
my $s = (gather { take 1; take 2 }); $s[0] = 5; say $s
    # raku:  X::Assignment::RO, "Cannot modify an immutable Int (1)"
    # mutsu: silently succeeds, and TRUNCATES -- prints `[5]`, not `[5 2]`
    #        (re-measured 2026-09-07; the truncation is new information, the
    #        store apparently reifies only as far as the assigned index)
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
