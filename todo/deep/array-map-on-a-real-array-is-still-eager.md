# `@a.map` on a real array is still eager — ADR-0058 step 2 does not cover it

**Re-scoped 2026-09-07 (second time today), and the new root cause is much
bigger than the two rows this file was narrowed to.** The previous framing said
the survivors were "a force-time `fail` under an enclosing `try`" and that
"nobody has explained rakudo's rule here yet". Measured against a build at
`e7662519d`, the rule needs no explaining: **the most common `.map` spelling of
all never became deferred.**

## The measurement

```raku
my @a = 1,2,3;
my $s1 = @a.map({ print "A"; $_ });        print "|";
my $s2 = (@a).map({ print "B"; $_ });      print "|";
my $s3 = @a.List.map({ print "C"; $_ });   print "|";
my $s4 = (1,2,3).map({ print "D"; $_ });   print "|";
print "\n";
$s1.List; $s2.List; $s3.List; $s4.List;
print "\n";
```

| | callbacks at the `.map` calls | callbacks at the pulls |
|---|---|---|
| rakudo | `\|\|\|\|` | (elided — rakudo sinks the four `.List` statements away) |
| mutsu | `AAA\|BBB\|\|\|` | `CCCDDD` |

So `@a.List.map` and `(1,2,3).map` **are** deferred, and `@a.map` / `(@a).map`
— a `.map` on an `ArrayKind::Array` value, i.e. on a real `@` array — still runs
its callback at the call.

## Why ADR-0058 step 2 missed it

Step 2 put the deferral in `dispatch_map_method`
(`runtime/methods_dispatch_match2.rs`). A `.map` on a **named array variable**
never gets there: it compiles to `OpCode::CallMethodMut`
(verified with `--dump-bytecode`: `GetArrayVar(0)`, `MakeAnonSub`,
`CallMethodMut { name_idx: "map", … }`), and that op has its own VM-native
array-method dispatch which runs the map eagerly. A `rust-gdb -batch` breakpoint
on `dispatch_map_method`'s deferred tail never fires for `@a.map({ … })`; one on
`eval_map_over_items` does not fire either, so the eager loop is a third
implementation, not either of the two the ADR knows about.

`t/map-callback-runs-at-consumption.t` (the ADR's own 23-row oracle, raku-verified
and green) does not catch this, so its spellings should be audited too.

## How this subsumes the old two rows

The old survivors were `try { … map … }` shapes whose exit status differed.
Re-measured, they are the same defect seen through `try`:

| shape | rakudo | mutsu |
|---|---|---|
| `sub ee { try { @a.map({ fail "boom" }) }; say "T"; 99 }` sunk | throws, `T` unreached | `T`, `Int`, alive |
| the same with `(1..3)` instead of `@a` | throws | `Failure`, `T` unreached |
| `my $s = try { … }; $s.List` (not sunk) | soft Failure, alive | same |
| `{ }` / `do { }` / no block instead of `try` | soft Failure, alive | same |

`try` implies `use fatal` (raku-doc `Language/exceptions.rakudoc`: "What `try`
actually causes is, via the `use fatal` pragma, an immediate throw"), and
`SeqSource::MapGrep` already captures `self.fatal_mode` at the `.map` call and
restores it around the pull, which is the right mechanism. mutsu's `fail` also
already throws under `use fatal` (measured: four control rows agree with raku).
The reason the `try` rows still diverge is that the `@a` spelling never builds a
`MapGrep` at all, so there is nothing to capture the `try`'s fatal-ness onto —
and for the `(1..3)` spelling the deferral goes through `make_lazy_pipe`
(`is_lazy_pipe_source`), a `LazyList`, which has no `fatal` field either.

**So the fix is not a rule about `try`.** It is: route every `.map` through one
deferral that carries `fatal`, which is exactly ADR-0058 step 4 (retire
`create_lazy_map_list`, and now also the `CallMethodMut` eager path and the
`make_lazy_pipe` path).

## What to do

1. Find the eager array-method map inside `OpCode::CallMethodMut`
   (`src/vm/vm_call_method_mut_ops.rs`) — `try_native_array_map` is *not* it
   (that one is the narrow rw/writeback loop and declines a read-only block).
   Make it produce the same `Value::seq_deferred(SeqSource::MapGrep { …, fatal:
   self.fatal_mode })` step 2 produces, or delegate to `dispatch_map_method`.
2. Decide what `make_lazy_pipe`'s `LazyList` does about `fatal` — either give it
   the field or route the finite case through `MapGrep`.
3. Audit `t/map-callback-runs-at-consumption.t`'s 23 rows for spelling coverage:
   add the `@a.map` form to every row that only exercises a List/Range receiver.
4. Then the two `todo` rows in that oracle ("a force-time `fail` under an
   enclosing `try` returns a Failure instead of throwing") should fall out; they
   are this file's completion signal.

This is a *behaviour* change on the hottest `.map` spelling there is, so
ADR-0058 §5's mandatory full local `make roast` applies — and expect it to find
consumers, as it did for step 3 (three of them, all general defects eager `map`
had been hiding: `news/2026-09/listop-map-defers.md`).

## Not part of this ticket

ADR-0058 step 3 (the listop form) landed 2026-09-07. Step 3b (`grep`) is tracked
in the ADR and in `todo/deep/`'s grep row; `grep` has the same eager-on-a-real-
array shape and should be checked at the same time.
