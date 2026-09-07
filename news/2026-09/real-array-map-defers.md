# `@a.map` on a real array defers too — ADR-0058 step 2's biggest hole is closed

`.map` on a **named `@` array** — the commonest `.map` spelling there is — was
still eager after ADR-0058 steps 2 and 3a. Measured at `2c7fc1dde`:

```raku
my @a = 1,2,3;
my $s1 = @a.map({ print "A"; $_ });        print "|";
my $s2 = (@a).map({ print "B"; $_ });      print "|";
my $s3 = @a.List.map({ print "C"; $_ });   print "|";
my $s4 = (1,2,3).map({ print "D"; $_ });   print "|";
print "\n";
$s1.List; $s2.List; $s3.List; $s4.List;
```

| | at the `.map` calls | at the pulls |
|---|---|---|
| rakudo | `\|\|\|\|` | `ABCD`-equivalent (elided; rakudo sinks the four `.List` statements) |
| mutsu **before** | `AAA\|BBB\|\|\|` | `CCCDDD` |
| mutsu **after** | `\|\|\|\|` | `AAABBBCCCDDD` |

## Root cause: not one eager loop but three, all keyed on the `@` receiver

ADR-0058 step 2 put the deferral in `dispatch_map_method`. A `.map` on a named
array variable never gets there — it compiles to `OpCode::CallMethodMut`, whose
dispatch reaches `call_method_mut_with_values` first. The ticket had already
established that neither the ADR's own deferred tail nor `eval_map_over_items`
fires for `@a.map({ … })`; what the debugger found is that **the eager loop was
three different loops**, selected by the callback's shape:

1. `Interpreter::call_method_mut_with_values`'s "map with rw binding" gate
   (`runtime/methods_mut_dispatch.rs`) — fired for **every** `.map` whose
   receiver name starts with `@`, read-only blocks included, and ran
   `eval_map_over_items_rw` on the spot.
2. `try_native_array_map` (`vm/vm_native_map.rs`) — the narrow loop that exists
   to capture a prefix `++$_`/`--$_` or a bare `tr///` through
   `rw_map_topic_capture`. The ticket said this one was "NOT it"; it declines a
   *read-only* block, which is why the ticket's read-only probe never reached
   it, but a writing block goes straight here.
3. `builtin_map`'s `source_var` branch — the listop twin,
   `map { $_++ }, @a`.

All three existed for the same reason: **Raku rw-binds `$_` to the source
element, so a `.map` on an `@` array can write back into it**, and the writeback
was published by re-binding the receiver's *name* in the current frame. A
deferred map is pulled wherever the Seq is consumed, where that frame is gone.

## The fix

`SeqSource::MapGrep` gains an `rw_source: Option<Value>` — the `@` container the
`.map` was called on. All three sites now hand back
`Value::seq_deferred(SeqSource::MapGrep { items, func, fatal, rw_source })`, and
`Interpreter::pull_rw_map` runs the rw loop at pull time: `try_native_rw_map_over`
(loop 2, now an eligibility check plus a loop that *returns* its writeback
instead of publishing it) first, `eval_map_over_items_rw` otherwise. Either way
the writeback is published by mutating the source `ArrayData` **in place**
(`publish_rw_map_writeback`) — frame-independent, reaches every alias by
construction, and keeps the container's element-type metadata that the old
`Value::real_array` / `Value::array_data_like` rebuilds had to re-register by
hand. This is the same move that made `grep`'s element promotion
frame-independent (`news/2026-09/grep-promotion-is-published-in-place.md`).
`Value::array_data_like` had no caller left and is gone.

rakudo agrees that the writeback lands at *consumption*:

```raku
my @a = 1,2,3;
my $s = @a.map({ $_++; $_ });
say "before-pull: ", @a;   # rakudo AND mutsu: [1 2 3]
say "pulled: ", $s.List;   # (2 3 4)
say "after-pull: ", @a;    # [2 3 4]
```

## The `try` rows closed with it, and they were not about `try`

`t/map-callback-runs-at-consumption.t`'s two remaining `todo` rows ("a
force-time `fail` under an enclosing `try` returns a Failure instead of
throwing") are gone, and the mechanism was not a rule about `try` — two
independent gaps had to close:

- A `...` **stub** callback took the older `create_lazy_map_list` `LazyList`
  detour, which has no `fatal` field. The stub's only requirement is "do not
  fire while the Seq is never iterated", which `SeqSource::MapGrep` provides, so
  `is_stub_routine_body` was dropped from both deferral predicates (in
  `dispatch_map_method` and `builtin_map`). Only `body_contains_return` still
  takes the `LazyList` route, for the out-of-dynamic-scope `return` it gets
  right; that is ADR-0058 step 4's remaining scope.
- `...` **is** `fail`, and a `fail` escapes as a `Control::Fail` error that the
  next routine boundary softens into a returned `Failure`. Under the `use fatal`
  that `try` implies, rakudo throws instead — and that boundary is nowhere near
  the pull: it is whichever routine encloses the *consumer*. The `MapGrep` pull,
  which is the one place that knows the call site's `fatal`, now turns a
  `Control::Fail` into a hard throw when it was captured true.

## `make_lazy_pipe` needed no `fatal` field, and had no finite case to reroute

The ticket asked for a decision on `make_lazy_pipe`'s `LazyList` route, on the
premise that the `(1..3)` `try` row went through it. It does not:
`is_lazy_pipe_source` is true only for a genuinely **infinite** source (an
`i64::MAX`-ended range, an infinite `GenericRange`, a lazy pipe, a gather-backed
`LazyList`), so a finite range has been a `MapGrep` since step 2 and there is no
finite case to reroute. The infinite route was measured directly instead —
`sub ee { my $s = try { (1..*).map({ fail "boom" }) }; say "T"; $s[0] }` prints
`T` and dies under both rakudo and mutsu — so it needs no `fatal` field either.

## What the gates found — eight more general defects eager `map` had hidden

ADR-0058 §5's mandatory full local `make roast` (plus `make test` and the
bundled-battery gate) paid for itself a third time. Every one of these is a
pre-existing bug that only became reachable once the commonest `.map` spelling
started producing a deferred body:

| where | defect | fix |
|---|---|---|
| `vm/vm_var_assign_set_local.rs` | `SeqBody::mark_itemized` was **unreachable**. A `$`-sigil local is stored in the locals table under its BARE name (`"s"` for `my $s`, while `@`/`%` keep their sigil), and the arm tested `name.starts_with('$')`. Its `LazyList` twin three lines below has always used the right test, which is why `my $s = map …; ` never forced while `map` produced a `LazyList`. | test "not `@`/`%`/`&`", as the twin does (`t/itemized-scalar-sink-does-not-force-lazy.t`, `t/signature-introspection-gaps.t`) |
| `vm/vm_helpers_lazy.rs` | The pull ran the callback under the **consumer's package**. `call_compiled_closure_in_unit` installs a Sub's declaring package when it is invoked from a foreign frame, but the map loop drives the block through `run_reuse`, which bypasses that — so `class Outer { our sub f(@n) { @n.map({ Inner.new }) } }` consumed from `GLOBAL` could not resolve `Inner`. | the pull installs the callback's own `SubData::package` guard (`t/closure-package-nested-class.t`) |
| `vm/vm_hyper_race_parallel.rs` | A hyper/race worker reified a nested lazy *pipe* result but not a nested deferred `MapGrep`, so `@a.hyper.map({ @ids.map({…}) }).flat` came out empty. | `reify_map_grep_seq` beside the existing `reify_finite_pipe_value` (`t/hyper-map-implicit-named-slurpy-leak.t`) |
| `vm/vm_var_multidim_ops.rs` | `multi_dim_index_read` walks elements purely, so `@a.map({ $_ })[*;*]` read ADR-0034's empty seed. | reify the receiver first (`t/seq-multidim-flatten.t`) |
| `vm/vm_var_assign_element.rs` | The Seq element store names the element it refused (`Cannot modify an immutable Int (2)`); an unpulled body named `Nil`. A `MapGrep` source is finite and stays re-readable, unlike the one-shot sources the surrounding guard protects. | reify before deciding the store (`t/producer-seq-named-receiver-write.t`) |
| `vm/vm_var_assign_typed.rs` | **String interpolation** rendered a deferred Seq as nothing: `"X{ @a.map({ $_ * 2 }) }Y"` came out `XY`. `StringConcat` is the one string path with no surrounding coercion op to hang §8.2's guard on, so it had none. Pre-existing since step 2 — `"{ (1,2,3).map({...}) }"` was equally empty — and only reached by a whitelisted test once `@a.map` deferred (`roast/S06-signature/positional.t` #5, a recursive `$i ~ "[{map { f($_, …) }, @a}]"`). | reify each interpolated value |
| `runtime/resolution_map_grep_rw.rs` | The rw loop merged the block's captured env into the running frame with **plain caller priority**, so a same-named lexical live in the CONSUMING frame shadowed the block's own free variable: `sub p($f) { my $c = C.new($f); @data.map: { $c.use } }` read the unit's `$c`. `eval_map_over_items` had already been given the capture-wins rule (ADR-0058 §9.1/§9.3); the rw loop had never needed it, because it only ever ran inside the frame that created the block. Caught by the **bundled-library gate** — `Text::CSV`'s `66_formula.t` and `Cro::HTTP`'s `http-cookiejar.rakutest`, neither of which `make test` nor roast reproduces. | the rule is now one shared `capture_wins_over_caller`, used by both loops so they cannot drift |
| `vm/vm_var_assign_index_named.rs` | The slice **INDEX** may itself be a deferred Seq (`@n[@n.map(*+0)] = <a b>.sort`): every reader walks it purely, so the store silently addressed no slots. Also pre-existing since step 2 (`@n[@n.List.map(*+0)]` fails the same way) — §8.2 had reified the RHS for a slice target but not the index. rakudo evaluates the index sequence first, which is exactly this pull. (`roast/S32-list/seq.t` #12/#14) | reify the popped index |

Making `mark_itemized` live also needed its companion: rakudo's `itemized`
exemption is about **sink context**, not about `.sink`. Measured,
`my $s = (1,2,3).Seq; $s.sink; $s.List` throws `X::Seq::Consumed` while the bare
mention `$s;` on the next line only warns "Useless use of $s in sink context",
so `SeqBody` grew `sink_explicit` for the `.sink` METHOD (both its dispatch
site and the pure-value native), and the implicit-sink call sites keep `sink`
(`t/seq-consumption-matrix.t`).

`t/array-map-seq.t`'s "topic-mutating map still writes back to the array" was
the one assertion that had been pinning mutsu's *eagerness*: it read `@b` after
`.^name`, which does not consume. Re-measured under `raku`, that program answers
`[1, 2, 3]`; the row now asserts that, and the write-back-once-consumed half is
pinned in the oracle file instead.

## The oracle grew a spelling audit

`t/map-callback-runs-at-consumption.t` is 37 rows now (was 23), still verified
37/37 under real `raku`, with **no `todo` rows left**. Part 3 is new: every Part
1/2 row has a List or Range receiver, which is exactly why they all passed while
`@a.map` stayed eager. Part 3 repeats those shapes on a real `@` array, plus the
listop-over-`@a` form and the deferred rw write-back timing.

## `grep` (ADR-0058 step 3b) is still eager, in every spelling

Measured on the same build for the ticket's "check it at the same time" note:

```raku
my @a = 1,2,3;
my $g1 = @a.grep({ print "A"; $_ > 1 });       print "|";
my $g2 = @a.List.grep({ print "C"; $_ > 1 });  print "|";
my $g3 = (1,2,3).grep({ print "D"; $_ > 1 });  print "|";
my $g4 = grep({ print "E"; $_ > 1 }, @a);      print "|";
# rakudo: ||||        mutsu: AAA|CCC|DDD|EEE|
```

So `grep` has no `@a`-specific hole to find: it is uniformly eager, and step 3b
already owns all four spellings. Its real-array arm (`dispatch_grep`'s
`ValueView::Array` branch) is the one that promotes matched slots to shared
`ContainerRef` cells, whose publication was already made frame-independent in
`news/2026-09/grep-promotion-is-published-in-place.md` — which is precisely the
prerequisite a deferred grep needs, for the same reason `rw_source` needed it
here.
