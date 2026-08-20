# ADR-0038: `.cache` returns a `List`, and the Seq/List view is a property of the value — read through one oracle

- **Status**: Accepted — implemented (phases 1-3 landed together; phase 4 optional cleanup deferred, see `todo/tickets/collapse-lazylist-cache-copies.md`)
- **Date**: 2026-08-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md) (gave `Seq` a real body and split reify from consume — this ADR fills the gap that `.cache`'s *return type* was never in its scope), [ADR-0030](0030-native-array-decode-cache-interior-mutability.md) (the `SyncUnsafeCell` primitive `SeqBody` already uses), [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) (the "one dispatch entry" ambition this ADR applies to type-name answering)
- **Ticket**: [todo/deep/seq-cache-does-not-narrow-to-list-stack-overflow.md](../../todo/deep/seq-cache-does-not-narrow-to-list-stack-overflow.md)

---

## 1. Context

### 1.1 The symptom: a Rust-level stack overflow in four files

Under `MUTSU_REAL_TEST=1` (the vendored upstream `Test.rakumod`, per
`todo/deep/vendor-real-test-module.md`), four files abort with `SIGABRT` / exit 134 and
`fatal runtime error: stack overflow`:

| File | Trigger |
| --- | --- |
| `roast/S16-io/words.t` | `is-deeply WORDS($fh), $all-words` where `WORDS` is `$fh.words` |
| `roast/S32-list/tail.t` (line 157) | `is-deeply Seq.new(class :: does Iterator {…}.new), <a b c>.Seq` |
| `roast/S32-io/io-cathandle.t` | `is-deeply` on `IO::CatHandle` `.lines` / `.handles` |
| `t/io-cathandle-lazy.t` | same |

A Rust abort is the highest-priority failure class the project recognises, and this is the largest
shared mechanism left in the real-`Test` campaign (recorded in PR #6715).

### 1.2 It is one mechanism, and the debugger names it exactly

`rust-gdb -batch -ex run -ex 'bt 80'` under `ulimit -s 2048` on `t/io-cathandle-lazy.t` shows a
strictly periodic 21-frame cycle, repeated to exhaustion:

```
#17 Interpreter::call_compiled_function_named_inner (… fn_package="Test", fn_name="is-deeply")
#20 Interpreter::dispatch_func_call_inner          (… name="is-deeply", args=Vec(size=3))
#21 Interpreter::exec_call_func_op                 (… name_idx=6, arity=4)
#38 Interpreter::call_compiled_function_named_inner (… fn_package="Test", fn_name="is-deeply")
#41 Interpreter::dispatch_func_call_inner          (… name="is-deeply", args=Vec(size=3))
…
```

Every frame carries the **same** `cf` pointer, the **same** `op_idx = 10`, and the **same**
`args=Vec(size=3)`. This is not deep-but-finite traversal of a data structure, and it is not lazy-IO
materialization: it is `Test::is-deeply` selecting *itself* forever. Nothing in mutsu's own lazy
machinery appears in the cycle.

### 1.3 Why `is-deeply` recurses: the Seq→List narrowing is its termination condition

`modules/Rakudo-Core/lib/Test.rakumod:609-618` (verbatim upstream):

```raku
multi sub is-deeply(Seq:D $got, Seq:D $expected, $reason = '') { is-deeply $got.cache, $expected.cache, $reason }
multi sub is-deeply(Seq:D $got, Mu    $expected, $reason = '') { is-deeply $got.cache, $expected,      $reason }
multi sub is-deeply(Mu    $got, Seq:D $expected, $reason = '') { is-deeply $got,       $expected.cache, $reason }
multi sub is-deeply(Mu    $got, Mu    $expected, $reason = '') { …the real comparison… }
```

The first three candidates are pure *narrowing* steps. They terminate only because rakudo guarantees
that **`Seq.cache` returns a `List`**, so the narrowed argument no longer binds `Seq:D` and dispatch
falls through to the `(Mu, Mu)` candidate. In mutsu, `.cache` on several Seq flavours yields
something that **still binds `Seq:D`**, so the same candidate is re-selected with an equivalent
argument list, forever.

This is worth stating plainly because it inverts the intuitive reading of the ticket that spawned
this work: the bug is **not** in `is-deeply`, and **not** in structural comparison recursing through
a lazy sequence. `Test.rakumod` is correct upstream code, and the recursion it writes is
well-founded *given the rakudo contract*. mutsu violates the contract; the unbounded recursion is
the downstream consequence.

### 1.4 The measured contract

`tmp/cache3.p6` (both interpreters, same file). `raku` answers `^name = List`, `~~ Seq:D = False`
for **every** row. mutsu:

| Seq flavour | internal repr | `.cache.^name` | `.cache ~~ Seq:D` | |
| --- | --- | --- | --- | --- |
| `(1,2,3).Seq`, `<a b c>.tail(*+10)`, `42.tail(*+0)` | reified `SeqBody` | `List` | `False` | correct |
| `map`/`grep` pipe, `gather` | `LazyList` (pipe) | `List` | `False` | correct |
| `IO::Handle.lines`, `IO::Handle.words`, `Seq.new($iterator)` | deferred `SeqBody` | **`Seq`** | **`True`** | **facet A** |
| `IO::CatHandle.lines`, `.handles` | cat-pull `LazyList` | `List` | **`True`** | **facet B** |

Two failing rows, two different representations, one violated contract. Each reproduces on its own
in a seven-line file that `raku` passes (`tmp/facetA.t`, `tmp/facetB.t` in the ticket) — so they are
independent defects, not one defect seen twice.

### 1.5 Facet A — a deferred `SeqBody` has no List view at all

`src/vm/vm_helpers_lazy.rs:297-299`, in `reify_or_consume_seq_target`:

```rust
if method == "cache" {
    return Ok(target);
}
```

`.cache` returns the receiver **unchanged**. Correct in as far as it goes — ADR-0034 made `.cache`
mean "reify in place, idempotently, do not consume", and returning the same body is how a caller
observes that. But the returned `Value` is still a `ValueView::Seq`, so both type oracles answer
`Seq`, and the rakudo contract is broken.

There is no cheap repair, because **a deferred `SeqBody` has nowhere to record "this handle is a
List view of that body."** `ValueView::Seq(Arc<SeqBody>)` is the only handle shape, and
`ADR-0034 §2` deliberately gave `SeqState` exactly four fields (`source`, `cached`, `lazy`,
`hyper`) plus the later `retained` — none of which is a view bit, because `.cache`'s *return type*
was never in that ADR's scope (it decided what `.cache` **does to** the Seq, not what it
**hands back**).

### 1.6 `.cache` must not force — measured, not assumed

The obvious repair for facet A — reify and return `Value::array(elements)` — is wrong. Against
`raku`, on a `Seq.new($infinite_iterator)`:

```
^name: List          <- printed immediately
is-lazy: …           <- hangs here
```

`.cache` answers `List` **before** anything is pulled. So the required return is *a `List` view over
a not-yet-reified body* — precisely the thing mutsu cannot currently express for `SeqBody`. Eager
materialization would also re-break `$*ARGFILES.lines` streaming and `Seq.from-loop`, which
ADR-0034 §3.4 already rejected on those grounds.

### 1.7 Facet B — the List view exists, but one of the two oracles ignores it

For a cat-pull `LazyList`, `.cache` *does* produce a List view: it returns a clone tagged with the
`__mutsu_lazylist_list_context` marker (`src/value/value_lazy.rs:269`,
`with_cached_no_sink().with_list_context()`). That is why `.^name` correctly says `List`.

mutsu then answers "what type is this value?" through **two independent tables that disagree**:

```rust
// src/runtime/utils/type_misc.rs:24-30  — value_type_name  (drives .^name / .WHAT / dispatch_mro)
ValueView::LazyList(ll) if ll.in_array_context() => "Array",
ValueView::LazyList(ll) if ll.in_list_context()  => "List",   // <-- checked FIRST
ValueView::LazyList(ll) if ll.is_cat_pull()      => "Seq",
ValueView::LazyList(ll) if ll.is_from_gather()   => "Seq",
ValueView::LazyList(_)                           => "Array",
```

```rust
// src/runtime/types/type_matching.rs:408-420  — type_matches_value's `tag_match` fast-accept
//   (drives `~~` and multi-dispatch signature binding)
ValueView::LazyList(list) if list.is_cat_pull() => constraint == "Seq",   // <-- no list-context arm
_ => false,
```

The second table is a hot-path *fast accept* that short-circuits before the general checker
(`if tag_match && !subsets.contains_key(constraint) { return true; }`). It keys off `is_cat_pull()`
alone and never consults the marker its sibling checks first. Multi-dispatch binds through
`type_matches_value`, so `is-deeply`'s `Seq:D` candidate keeps matching a value whose `.^name` is
`List`. A single value simultaneously *is* and *is not* a `Seq`, depending on who asks.

Two further disagreements found while confirming this, both latent:

- `src/runtime/methods_introspect.rs:65-70` is a **third** copy of the same table, and its default
  arm is `"Seq"` where `type_misc.rs`'s is `"Array"`; it has no `is_cat_pull` arm at all.
- The `.cache` LazyList arm (`ll.is_genuinely_lazy() || ll.is_cat_pull()` →
  `with_cached_no_sink().with_list_context()`) is **copy-pasted at five sites**:
  `src/builtins/methods_0arg/collection.rs:1181`, `src/runtime/methods_call_dispatch.rs:3558`,
  `src/vm/vm_call_method_ops.rs:1139`, `src/vm/vm_call_method_mut_ops.rs:788`,
  `src/vm/vm_native_dispatch.rs:167`.

### 1.8 The structural finding

"Is this value a `Seq` or a `List`?" currently has **three** homes and **three** readers, and no two
of them agree:

| Home | Applies to | Read by |
| --- | --- | --- |
| the `ValueView` variant (`Seq` vs `Array`) | reified Seqs, plain lists | all three readers |
| a stringly-keyed marker in the value's captured closure `env` (`__mutsu_lazylist_list_context`) | `LazyList` | `value_type_name`, `methods_introspect` — **not** `type_matches_value` |
| *nothing* | deferred `SeqBody` | — |

A magic string in a closure environment is not a type tag, and a fast-accept table that reimplements
a subset of the naming table is not a type system. This ADR's second half is about collapsing that.

---

## 2. Decision

**Two changes, in this order.**

**(1) The Seq/List *view* is a property of the value, expressed in one place per representation.**
`SeqBody` gains the view bit it lacks; `LazyList`'s env marker is promoted to a typed field.

```rust
// src/value/seq_body.rs — added to SeqState (ADR-0034 §2)
struct SeqState {
    source: SeqSource,
    cached: bool,
    lazy: bool,
    hyper: Option<(Option<i64>, Option<i64>)>,
    retained: bool,
    /// Which Raku type this *handle* presents as. `.cache`/`.List` return a
    /// second `Value` sharing this same `Arc<SeqBody>` with `view = List`;
    /// reification state, source, and elements are shared and unaffected.
    view: SeqView,   // Seq | List | Array
}
```

The view belongs to the **handle**, not the body, so a `.cache` result must be a distinct `Value`
that shares the body. Two shapes satisfy that; §3 picks between them.

**(2) One oracle answers "what Raku type is this value".** `value_type_name` is authoritative.
`type_matches_value`'s `tag_match` stops carrying its own `LazyList`/`Seq` arm and defers to it;
`methods_introspect`'s third copy is deleted in favour of it. The rule to write down, in the
ADR-0030 §6 style: *a site that needs a value's Raku type name calls `value_type_name`; if it needs
a faster answer than that, it makes `value_type_name` faster.*

With both in place, `.cache` on every flavour returns a value whose single type name is `List`,
`is-deeply`'s narrowing terminates, and the four files stop aborting.

### 2.1 Scope note: this is a gap in ADR-0034, not a reversal of it

ADR-0034 stands entirely. It decided *what reification does to a Seq* (fills it in place;
idempotent; distinct from consumption) and shipped that. It never decided *what `.cache` returns*,
because the four files in §1.1 were not in its evidence set — its §7.1 explicitly records that the
§1.5 site list was incomplete and that two more bugs surfaced only end-to-end. This is the third.
Per `docs/adr/README.md`, ADR-0034 is not rewritten; this ADR is a separate decision at the same
layer.

---

## 3. Options considered

| # | Option | Fixes A? | Fixes B? | Keeps `.cache` non-forcing? | Blast radius | Verdict |
| --- | --- | --- | --- | --- | --- | --- |
| 0 | Do nothing; leave the four files failing | ✗ | ✗ | — | 0 | Rejected — §3.1 |
| 1 | Add the list-context arm to `type_matching.rs:418` only | ✗ | ✓ | ✓ | 2 lines | Partial — §3.2 |
| 2 | `.cache` reifies eagerly and returns `Value::array` | ✓ | ✓ | **✗** | small | Rejected — §1.6 |
| 3 | View bit on `SeqState` + `LazyList` field, one oracle | ✓ | ✓ | ✓ | ~15 sites | **Recommended** |
| 4 | A distinct `ValueView::SeqListView(Arc<SeqBody>)` variant | ✓ | ✓ | ✓ | ~330 sites | Rejected — §3.3 |
| 5 | Special-case `is-deeply` / add a recursion depth cap | ✗ | ✗ | — | small | Rejected — §3.4 |

### 3.1 Why "do nothing" is not available

Nothing whitelisted depends on `MUTSU_REAL_TEST=1` today, so this blocks no current roast number.
It blocks the campaign that retires mutsu's native `Test` provider (BATTERIES.md rung 2), and a
Rust abort is a failure class the project treats as top priority regardless of gating. It is also
cheap to leave half-fixed and expensive to leave wholly unfixed, which is what §3.2 is about.

### 3.2 Option 1 is correct, insufficient, and should ship first anyway

Adding the missing arm at `type_matching.rs:418` is right on its own terms — the two tables should
not disagree, independent of `is-deeply`. It fixes facet B, i.e. two of the four files, in about two
lines. It does **not** touch facet A, because a deferred `SeqBody` has no marker for the arm to
read (§1.5).

It is listed as `Partial` rather than `Rejected` because sequencing it first is a genuine benefit
and not a band-aid: it removes an oracle disagreement that would otherwise have to be reasoned
about *while* changing the representation, and it halves the failing set immediately. The risk to
watch is the opposite of the usual one — that shipping it is mistaken for closing the ticket. Two
files will still abort; the ticket stays open until §4's phase 3.

### 3.3 Why not a new `ValueView` variant for the List view

`ValueView::SeqListView(Arc<SeqBody>)` would make the view unmissable at every match site — which is
also the objection: there are ~330 `ValueView::Seq(..)` sites (ADR-0034 §2.1), and every one would
have to decide whether it also means the new variant. ADR-0034 spent its entire blast-radius budget
on *avoiding* that (the `Deref` trick), and re-spending it to carry one enum field is not
proportionate. A field inside the existing payload is invisible to sites that do not care, which is
the correct default here: element reads, `Trace`, equality, and the JIT all behave identically for
both views.

### 3.4 Why not cap the recursion or special-case `is-deeply`

A depth cap converts a stack overflow into a thrown exception. That is strictly better crash
behaviour and worth having *eventually* as defence in depth, but as a fix here it is a
test-visible-symptom patch: `is-deeply` would report a failure instead of aborting, while still
comparing the wrong things, and every other consumer of the rakudo `.cache` contract (`Test::Util`'s
`is-eqv` already `.cache`s both sides — ADR-0034 §7.1) stays broken silently. Special-casing
`is-deeply` is worse still: it is upstream code that mutsu is supposed to run verbatim (BATTERIES.md
§1), and the whole point of the real-`Test` campaign is that mutsu bends, not the module.

---

## 4. Migration plan

| # | Phase | Files | Closes |
| --- | --- | --- | --- |
| **1** | Land the oracle-agreement test **first**: a `t/seq-cache-returns-list.t` generated from the §1.4 matrix, asserting for every flavour that `.cache.^name eq 'List'`, `.cache ~~ List:D`, `!(.cache ~~ Seq:D)`, and that `.cache` did not force (`.^name` answers without pulling an infinite source). Mark the two failing rows `todo`. | `t/` only | nothing — pins the target |
| **2** | Facet B (option 1): make `type_matches_value`'s `tag_match` defer to `value_type_name` for `ValueView::LazyList` instead of carrying its own `is_cat_pull` arm. Delete `methods_introspect.rs:65-70`'s third copy in the same move, or record why it must differ. | `src/runtime/types/type_matching.rs`, `src/runtime/methods_introspect.rs` | `roast/S32-io/io-cathandle.t`, `t/io-cathandle-lazy.t` |
| **3** | Facet A: add `SeqView` to `SeqState`; add `Value::seq_list_view(body)` returning a handle sharing the `Arc<SeqBody>` with `view = List`; make `vm_helpers_lazy.rs:297`'s `.cache` return it; teach `value_type_name` to read it. | `src/value/seq_body.rs`, `src/value/mod.rs`, `src/vm/vm_helpers_lazy.rs`, `src/runtime/utils/type_misc.rs` | `roast/S16-io/words.t`, `roast/S32-list/tail.t` — **closes the ticket** |
| **4** | Collapse the five copy-pasted `.cache` LazyList arms (§1.7) into one helper, and promote `__mutsu_lazylist_list_context` / `__mutsu_lazylist_cached_no_sink` / the array-context marker from env magic strings to typed `LazyList` fields. | `src/value/value_lazy.rs` + the five sites | the §1.8 structural finding |

Phases 2 and 3 are separately shippable and separately verifiable. Phase 4 is cleanup and carries no
behaviour change; it may be dropped without reopening the ticket, but not without re-recording the
finding.

### Verification

```sh
ulimit -s 2048    # makes the overflow reproduce in seconds rather than ~30s
for f in roast/S16-io/words.t roast/S32-io/io-cathandle.t roast/S32-list/tail.t t/io-cathandle-lazy.t; do
  MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 timeout 120 target/debug/mutsu "$f"; echo "$f exit=$?"
done                                            # all four must leave exit 134
raku tmp/cache3.p6; target/debug/mutsu tmp/cache3.p6   # must agree row for row
prove -e target/debug/mutsu t/seq-*.t t/lazy-seq-*.t t/io-cathandle-lazy.t
```

---

## 5. Risks

- **Phase 3 changes what `.cache` returns, and `.cache` is on the dispatch path of a lot of lazy
  code.** Anything that today relies on `$s.cache` still *being* `$s` (identity, or a following
  Seq-only method) changes behaviour. ADR-0034 §7.1's `retained` flag interacts directly: the List
  handle and the Seq handle share one body, so a `retained` set through either must be visible
  through both. Getting that backwards silently re-breaks `X::Seq::Consumed` timing, which has no
  test signal outside `t/seq-consumption-matrix.t` — extend that file in phase 3, do not just run it.
- **Phase 2 makes `type_matches_value` slower on a hot path.** The `tag_match` block exists to
  short-circuit the "ubiquitous `Int $n` / `Point $p` params" gauntlet; routing `LazyList` through
  `value_type_name` adds a call on a path that currently answers from one `match`. It should be
  unmeasurable (the `LazyList` arm is not the common case, and the `Int`/`Str`/`Instance` arms are
  untouched), but if a bench row moves, the answer is to make `value_type_name` inlineable, not to
  restore the second table.
- **The §1.4 matrix may not be exhaustive.** It was assembled by enumerating the Seq-producing
  constructs reachable from the four failing files plus the obvious neighbours. A flavour that
  reaches `.cache` by a sixth route (§1.7 found five) would still be wrong after phase 3. Phase 1's
  generated test is the mitigation, and it should be generated from a `raku` run rather than
  hand-written, exactly as `t/seq-consumption-matrix.t` was.
- **Facet A's fix could tempt an eager-materialization shortcut** under time pressure, since it
  makes the tests pass. §1.6 measured that this is wrong; re-measure before believing otherwise.

---

## 6. Scope: what this ADR deliberately does not decide

- **A recursion-depth guard for compiled function dispatch.** §3.4 rejects it *as the fix*, but a VM
  that turns unbounded Raku-level recursion into `X::StackOverflow` rather than `SIGABRT` is a
  separate, defensible improvement — every one of the four files would then have produced a
  diagnostic instead of a core dump, and the investigation would have started from the right place.
  Worth its own ticket; it is not a substitute for the contract fix and must not be bundled.
- **`eqv` on two structurally-equal Seqs of different origin** (`$lines eqv ("a","b","c")` answers
  `False` in mutsu). Noted in the predecessor ticket as a separate, smaller correctness question and
  still open; it is not on the `is-deeply` path (the `(Mu, Mu)` candidate does its own walk).
- **Whether `LazyList` and `SeqBody` should be one representation.** ADR-0034 §6 held `LazyList` out
  of scope and that still holds. This ADR makes the two *agree* about their view bit; merging them
  is a much larger question that neither ADR's evidence supports opening yet.

---

## 7. Consequences

- **`Seq.cache` gets a contract**: it returns a `List` view, it does not force, and the view is
  recorded in the value. Written down and pinned by a generated test, so drift is a CI failure.
- **One oracle answers "what type is this value"**, and the fast-accept table stops being a place
  where a second, partial type system can accumulate.
- **Four `SIGABRT`s become passes**, and the real-`Test` campaign loses its largest remaining shared
  mechanism (PR #6715's classification).
- **`SeqBody` gains the field ADR-0034 would have given it had `.cache`'s return type been in
  scope** — the view bit sits next to `cached`/`retained`, which is where a reader looking for it
  will go.
- **If rejected**: the four files keep aborting, the two type oracles keep disagreeing (a bug that
  outlives `is-deeply` — any `multi` with a `Seq:D`/`List:D` pair of candidates can mis-dispatch on
  a cat-pull value), and the next lazy-sequence consumer adds a sixth copy of the `.cache` arm.

---

## 8. Open questions for the deciders

1. **Should the List view be a field on `SeqState`, or a separate handle type that wraps
   `Arc<SeqBody>`?** §2 assumes the field. A wrapper keeps `SeqState` about the *body* and the view
   about the *handle*, which is conceptually cleaner, but needs a new `ValueView` shape and so
   collides with §3.3's objection. **Recommendation: field on `SeqState`, read only through
   `value_type_name`; revisit if a single body ever needs two live views with different bits.**
2. **Should phase 2 ship as its own PR ahead of phase 3, or should they land together?** Phase 2 is
   two lines and fixes half the files; phase 3 is the representational change. ADR-0034's open
   question 1 faced the same fork and chose "together, to avoid the worst intermediate state" — but
   that was two halves of *one* mechanism, whereas these are genuinely independent defects.
   **Recommendation: separate PRs, phase 2 first, with the ticket explicitly left open.**
3. **Is `is_from_gather()` → `"Seq"` (`type_misc.rs:29`) also missing from the matching table, and
   does it have the same bug?** `gather` came out correct in §1.4, which suggests the list-context
   arm already covers it — but it was not probed under a `multi` with `Seq:D` candidates.
   **Recommendation: add a gather row to phase 1's generated test before assuming it is fine.**

---

*This ADR is `Proposed`. It records a gap in [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md),
which remains `Accepted` and is not modified. If the mechanism judgment here changes, supersede this
ADR rather than rewriting it.*
