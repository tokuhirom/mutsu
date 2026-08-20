# `Seq.cache` does not narrow to `List`, so the real `Test.rakumod`'s `is-deeply` recurses until the Rust stack overflows

**Status: root-caused 2026-08-20. Design recorded in
[ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md).
Ready for implementation — see "What to do next" below.**

Previously filed as `todo/deep/cathandle-real-test-is-deeply-infinite-recursion.md`, and before that
as `todo/tickets/cathandle-handles-wrongly-lazy-array.md`. Renamed twice because the scope kept
turning out larger than its framing: it is not about `IO::CatHandle`, and it is not about
`is-deeply`.

## Symptom

Under `MUTSU_REAL_TEST=1` (the vendored upstream `Test.rakumod`), **four** files abort with
`SIGABRT` / exit 134 and `fatal runtime error: stack overflow` — the largest shared mechanism
remaining in the real-`Test` campaign, per PR #6715:

| File | Trigger |
| --- | --- |
| `roast/S16-io/words.t` | `is-deeply WORDS($fh), $all-words` (`WORDS` is `$fh.words`) |
| `roast/S32-list/tail.t` (line 157) | `is-deeply Seq.new(class :: does Iterator {…}.new), <a b c>.Seq` |
| `roast/S32-io/io-cathandle.t` | `is-deeply` on `IO::CatHandle.lines` / `.handles` |
| `t/io-cathandle-lazy.t` | same |

All four reproduce on current `main` (verified 2026-08-20, debug build, 4/4). `ulimit -s 2048` makes
them crash in seconds instead of ~30s.

## Root cause

**`Seq.cache` must return a `List`. In mutsu it returns something that still binds `Seq:D`.**

`modules/Rakudo-Core/lib/Test.rakumod:609-618` narrows Seq arguments before comparing:

```raku
multi sub is-deeply(Seq:D $got, Mu $expected, $reason = '') { is-deeply $got.cache, $expected, $reason }
…
multi sub is-deeply(Mu    $got, Mu $expected, $reason = '') { …the real comparison… }
```

The Seq→List narrowing **is the recursion's termination condition**. When `.cache` fails to narrow,
the same candidate is re-selected forever. `rust-gdb -batch -ex run -ex 'bt 80'` shows a strictly
periodic 21-frame cycle with the same `cf` pointer, the same `op_idx = 10`, and the same
`args=Vec(size=3)` in every repetition — `Test::is-deeply` calling itself, with none of mutsu's lazy
machinery in the cycle.

So: the bug is **not** in `is-deeply` (correct upstream code), **not** in structural comparison
walking a lazy Seq, and **not** in cathandle materialization. Those were the earlier hypotheses in
this file's history; all three are wrong.

### The measured matrix

`tmp/cache3.p6`, both interpreters. `raku` answers `^name = List`, `~~ Seq:D = False` for **every**
row. mutsu:

| Seq flavour | internal repr | `.cache.^name` | `.cache ~~ Seq:D` | |
| --- | --- | --- | --- | --- |
| `(1,2,3).Seq`, `<a b c>.tail(*+10)`, `42.tail(*+0)` | reified `SeqBody` | `List` | `False` | ok |
| `map`/`grep` pipe, `gather` | `LazyList` (pipe) | `List` | `False` | ok |
| `IO::Handle.lines`, `.words`, `Seq.new($iterator)` | deferred `SeqBody` | **`Seq`** | **`True`** | **facet A** |
| `IO::CatHandle.lines`, `.handles` | cat-pull `LazyList` | `List` | **`True`** | **facet B** |

Two independent defects — each reproduces alone in a seven-line file that `raku` passes:

- **Facet A** — `src/vm/vm_helpers_lazy.rs:297-299`: `if method == "cache" { return Ok(target); }`
  returns the receiver Seq unchanged. A deferred `SeqBody` has **nowhere to record a List view**:
  `SeqState` (ADR-0034 §2) has `source`/`cached`/`lazy`/`hyper`/`retained` and no view bit, because
  `.cache`'s *return type* was never in ADR-0034's scope. Hits `words.t`, `tail.t`.
- **Facet B** — the List view exists (`.cache` tags `__mutsu_lazylist_list_context`, which is why
  `.^name` correctly says `List`), but mutsu has **two type oracles that disagree**:
  `src/runtime/utils/type_misc.rs:24-30` checks `in_list_context()` **before** `is_cat_pull()`,
  while `src/runtime/types/type_matching.rs:418`'s hot-path `tag_match` fast-accept answers
  `constraint == "Seq"` for any cat-pull LazyList and never consults the marker. Multi-dispatch
  binds through the latter. Hits `io-cathandle.t`, `io-cathandle-lazy.t`.

### Do not "fix" this by materializing eagerly

Measured against `raku` on a `Seq.new($infinite_iterator)`: `.cache.^name` prints `List`
**immediately, before anything is pulled**. The required return is a `List` view over a
*not-yet-reified* body. Eager materialization also re-breaks `$*ARGFILES.lines` streaming and
`Seq.from-loop` (already rejected in ADR-0034 §3.4).

### Two things found while confirming, both latent

- `src/runtime/methods_introspect.rs:65-70` is a **third** copy of the type-name table; its default
  arm is `"Seq"` where `type_misc.rs`'s is `"Array"`, and it has no `is_cat_pull` arm.
- The `.cache` LazyList arm is **copy-pasted at five sites**:
  `src/builtins/methods_0arg/collection.rs:1181`, `src/runtime/methods_call_dispatch.rs:3558`,
  `src/vm/vm_call_method_ops.rs:1139`, `src/vm/vm_call_method_mut_ops.rs:788`,
  `src/vm/vm_native_dispatch.rs:167`.

## What to do next

Ordered; phases 2 and 3 are independently shippable. Full reasoning, rejected options, and risks in
[ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md) §4.

1. **`t/seq-cache-returns-list.t`** — generate the matrix above from a `raku` run; assert per
   flavour that `.cache.^name eq 'List'`, `.cache ~~ List:D`, `!(.cache ~~ Seq:D)`, and that `.cache`
   does not force. Mark the two failing rows `todo`.
2. **Facet B (~2 lines, fixes 2 of 4 files).** Make `type_matches_value`'s `tag_match`
   (`src/runtime/types/type_matching.rs:408-420`) defer to `value_type_name` for
   `ValueView::LazyList` rather than carrying its own `is_cat_pull` arm; delete
   `methods_introspect.rs:65-70`'s third copy in the same move. **This does not close the ticket** —
   `words.t` and `tail.t` still abort.
3. **Facet A (closes the ticket).** Add a `view: SeqView` field to `SeqState`; add a
   `Value::seq_list_view(body)` handle sharing the same `Arc<SeqBody>` with `view = List`; return it
   from `vm_helpers_lazy.rs:297`; teach `value_type_name` to read it. Extend
   `t/seq-consumption-matrix.t` — the List and Seq handles share one body, so ADR-0034's `retained`
   flag must stay visible through both.
4. **Optional cleanup.** Collapse the five `.cache` LazyList copies; promote the three
   `__mutsu_lazylist_*` env magic strings to typed `LazyList` fields.

## Verification

```sh
ulimit -s 2048
for f in roast/S16-io/words.t roast/S32-io/io-cathandle.t roast/S32-list/tail.t t/io-cathandle-lazy.t; do
  MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 timeout 120 target/debug/mutsu "$f"; echo "$f exit=$?"
done                                    # today: all four exit 134
raku tmp/cache3.p6; target/debug/mutsu tmp/cache3.p6    # must agree row for row
```

## Stale claims retired from earlier versions of this file

- *"`IO::CatHandle.handles` is wrongly lazy and wrongly an Array"* — fixed; `.handles` reports `Seq`
  and is not lazy externally.
- *"The trigger needs a Seq specifically backed by `LazyList::new_cat_pull`"* — false.
  `roast/S32-list/tail.t` has no CatHandle at all (a user `Iterator`), and `roast/S16-io/words.t`
  uses a plain `IO::Handle`.
- *"A reflective/attribute-walking comparator loops back into a shared CatHandle attribute cell"* —
  false; the debugger shows no attribute walk, only `is-deeply` re-dispatch.
- *"One run under `rust-gdb` completed normally, so there may be non-determinism"* — not reproduced.
  Under `ulimit -s 2048` the crash is deterministic under gdb too (SIGSEGV on the guard page, with
  the periodic backtrace above). Treat as fully deterministic.

## Repro files (not committed; recreate as needed)

`tmp/facetA.t` — deferred `SeqBody`, no CatHandle:

```raku
use Test;
plan 1;
is-deeply Seq.new(class :: does Iterator {
    has @!stuff = <a b c>;
    method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
}.new), <a b c>.Seq, 'deferred SeqBody is-deeply';
```

`tmp/facetB.t` — cat-pull `LazyList`, no deferred `SeqBody`:

```raku
use Test;
plan 1;
my $p = $*TMPDIR.add("mutsu-facetB-{$*PID}");
$p.spurt("a\nb\nc\n");
is-deeply IO::CatHandle.new($p).lines, ("a", "b", "c"), 'cat-pull LazyList is-deeply';
$p.unlink;
```

Both: `raku` passes; `MUTSU_REAL_TEST=1 target/debug/mutsu <file>` exits 134.

## Related

- [ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md) — the design.
- [ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md) — gave
  `Seq` a real body; this is the gap it left. Not superseded.
- `todo/deep/vendor-real-test-module.md` — the campaign this blocks.
- ADR-0038 §6 records two deliberate non-goals: a recursion-depth guard that would turn unbounded
  Raku recursion into `X::StackOverflow` instead of `SIGABRT` (worth its own ticket), and the
  separate `eqv`-on-two-Seqs question.
