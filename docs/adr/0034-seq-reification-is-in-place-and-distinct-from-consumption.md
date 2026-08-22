# ADR-0034: Reifying a `Seq` fills the Seq itself — reification and consumption are two operations, not one

- **Status**: Accepted — implemented (2026-08-19/20, all 5 phases; see §7.1 Outcome)
- **Date**: 2026-08-19
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0030](0030-native-array-decode-cache-interior-mutability.md) (the `SyncUnsafeCell` primitive and the generation-graveyard technique this ADR reuses verbatim), [ADR-0013](0013-container-interior-mutability-cellvalue.md) (the *other* interior-mutability primitive, and why it does not apply here), [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (the precedent for giving a container a real body instead of a bare payload), [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (the collector that currently cannot see a deferred Seq's iterator)
- **Ticket**: [todo/deep/deferred-seq-materialization-destroys-the-original.md](../../todo/deep/deferred-seq-materialization-destroys-the-original.md)

---

## 1. Context

### 1.1 The symptom

A second method call on a lazy `Seq` throws where rakudo answers. Merely *asking about* the
value is enough:

```raku
my $p = "tmp/lz.txt".IO;
$p.spurt("A\nB\nC\n");
my $b = $p.open(:r).lines;
say $b.defined;              # True (both)
say "Str: '" ~ $b.Str ~ "'"; # raku: 'A B C'   mutsu: X::Seq::Consumed
```

### 1.2 The ticket's stated root cause is not the one this repro exercises

The ticket blames the deferred-iterator arm in `src/runtime/methods_call_dispatch.rs` (~line 527),
which pulls every item into a **new** `Arc`, builds a **new** `Seq`, and re-dispatches on it. That
arm really does have this defect (§1.3, probe 5), but it is **not what the headline repro hits**.
Two `rust-gdb -batch` breakpoints settle it: on the repro above,
`mutsu::value::seq_take_deferred_iter` and `mutsu::value::seq_consume` are **never called**. The
site that actually fires is:

```
#0 Interpreter::force_if_lazy_io_lines            src/vm/vm_helpers_lazy.rs:14
#1 Interpreter::exec_call_method_mut_op_impl      src/vm/vm_call_method_mut_ops.rs:656
```

`IO::Handle.lines` does **not** produce a deferred-iterator `Seq`. It produces a
`ValueView::LazyIoLines { handle, kv, words, consumed: Arc<AtomicBool> }` — a *third*
representation of a lazy sequence, with its own consumed flag and its own force site:

```rust
// src/vm/vm_helpers_lazy.rs
if consumed.swap(true, Ordering::AcqRel) {
    return Err(crate::value::seq_consumed_error());
}
let forced = loan_env!(self, force_lazy_io_lines(handle, words))?;
```

`force_if_lazy_io_lines` returns the reified `Seq` **for the call only**. The receiver variable is
left holding the original `LazyIoLines` with `consumed == true`, so the *next* touch of the same
value throws. The exemption list guarding the call
(`vm_call_method_mut_ops.rs:651-655`: `kv | iterator | lazy | WHAT | ^name | does | isa`) is why
`.WHAT` survives and `.defined` does not — which is exactly backwards from rakudo (§1.4).

This correction matters for implementation planning: **a fix confined to
`methods_call_dispatch.rs` cannot close this ticket.** The ticket's own note that "the `.defined`
exemption had no observable effect … which suggests `.defined` reaches the reification through a
different path — worth locating before touching this arm at all" is confirmed here, and the
different path is `LazyIoLines`.

### 1.3 Both representations have the same defect, in two different flavours

`tmp/probe2.raku`, run against `raku` and `target/debug/mutsu`:

| # | Program shape | raku | mutsu |
| --- | --- | --- | --- |
| 1 | `my $a = $p.open(:r).lines; $a.cache; $a.List` | `A,B,C` | `A,B,C` |
| 2 | `sub c($s){$s.cache}; my $b = …lines; c($b); $b.List` | `A,B,C` | **THREW** |
| 3 | `my $d = …lines; my $e = $d; $d.cache; $e.List` | `A,B,C` | **THREW** |
| 5 | `my $s = Seq.new(It.new); $s.Str; $s.Str` (user `Iterator`) | `0 1 2` / `0 1 2` | `0 1 2` / **`''`** |

Row 1 passes only because of a deliberate band-aid added when `.cache` was fixed
(`news/2026-08/cache-makes-a-lazy-io-lines-seq-repeatable.md`): the reified `Seq` is written back
**over the receiver's name** in the caller's env
(`vm_call_method_mut_ops.rs:667-670`, `self.env_mut().insert(target_name.clone(), forced)`).

Rows 2 and 3 are the measurement that decides this ADR. A **name**-keyed writeback cannot reach a
value that is one call frame away (row 2) or that has a second alias (row 3), because those are
properties of the *value*, not of any name. Widening the band-aid to more methods (§3, option 0)
would fix the headline repro and still fail rows 2 and 3 — reification has to land in something
every alias shares, i.e. in the value itself.

Row 5 is the deferred-`Seq` flavour the ticket describes, and it is **worse than a throw**: the
second `.Str` silently yields the empty string. Wrong data beats a diagnostic here only in the sense
that it is harder to notice.

### 1.4 What rakudo actually does — the measured oracle

`tmp/matrix.raku` builds a fresh `$p.open(:r).lines` per row, calls one method, then asks whether
`$s.List` still works:

| Method | raku | mutsu |
| --- | --- | --- |
| `.Str` `.gist` `.raku` `.defined` `.DEFINITE` `.Bool` `.so` `.elems` `.cache` `.WHAT` `.WHICH` `.is-lazy` | **keeps** | **consumes** (all of them) |
| `.List` `.list` `.Array` `.eager` `.flat` `.sort` `.reverse` `.join` `.head` `.tail` `.first` `.sum` `.min` `.max` `.map` `.grep` `.pick` `.roll` `.unique` `.kv` `.pairs` `.Slip` `.Set` `.Bag` `.hyper` `.race` `.lazy` `.sink` `for` | consumes | consumes |
| `.iterator` | consumes | **keeps** |

The rule behind the left column is not a list — it is rakudo's `Seq` having **two** primitive
operations where mutsu has one:

- **`.cache`** reifies the iterator **into the Seq** (`$!list`). Idempotent. Everything in the
  first row routes through it, which is why none of them consume.
- **`.iterator`** / **`.list`** *steal* `$!iter` and hand it away without storing anything.
  Everything in the second row routes through one of those, which is why all of them consume.

mutsu has only "materialize", which does both at once: it pulls the iterator *and* discards the
container it pulled it from. So mutsu's default is "consume", with a hand-maintained exemption list
bolted on — and the exemption list is the wrong shape by construction, since it has to enumerate
the *majority* case. mutsu currently maintains **four** such lists, none of which agree:

| List | Location |
| --- | --- |
| `seq_deferred_method_keeps_lazy` (14 methods) | `src/value/mod.rs:150` |
| `matches!(method, "cache" \| "sink" \| "raku" \| "perl")` | `src/runtime/methods_call_dispatch.rs:527` |
| `consumed_methods` (9 methods) | `src/runtime/methods_call_dispatch.rs:555` |
| the `LazyIoLines` exemption (7 methods) | `src/vm/vm_call_method_mut_ops.rs:651` |

### 1.5 Three representations of one Raku type, and a fourth list of sites that reify

`.WHAT` says `Seq` for all of these, but internally mutsu has:

- `ValueView::Seq(ArcRef<Vec<Value>>)` **plus** a deferred iterator in a global side table;
- `ValueView::LazyIoLines { handle, kv, words, consumed }` (§1.2);
- `ValueView::LazyList(..)` — `gather`, `map`/`grep` pipes, sequence specs. **Out of scope** (§6).

and **ten** independent sites that decide to reify, each with its own guard:

| Site | File |
| --- | --- |
| `materialize_deferred_seq` | `src/vm/vm_coerce_concat_ops.rs:36` |
| deferred-Seq re-dispatch arm | `src/runtime/methods_call_dispatch.rs:503-551` |
| consumed-Seq guard | `src/runtime/methods_call_dispatch.rs:552-564` |
| `force_if_lazy_io_lines` + `.cache` writeback | `src/vm/vm_call_method_mut_ops.rs:650-674` |
| mut-path deferred-Seq reify | `src/vm/vm_call_method_mut_ops.rs:688-701` |
| native-dispatch bail | `src/vm/vm_native_dispatch.rs:107-113` |
| compiled/interpret bail | `src/vm/vm_call_method_compiled_interpret.rs:51` |
| `@`/`%` assignment reify | `src/vm/vm_var_assign_set_local.rs:377-387` |
| `for`-loop reify | `src/vm/vm_for_loop_dispatch.rs:231` |
| `.sink` fall-through | `src/builtins/methods_0arg/dispatch_core_math.rs:632-644` |

### 1.6 The side tables are a leak and an ABA hazard

Five global tables key Seq state by the element `Arc`:

```rust
static CACHED_SEQS:        OnceLock<Mutex<Vec<Weak<Vec<Value>>>>>;
static CONSUMED_SEQS:      OnceLock<Mutex<Vec<Weak<Vec<Value>>>>>;
static LAZY_SEQS:          OnceLock<Mutex<Vec<Weak<Vec<Value>>>>>;
static HYPER_CONFIGS:      OnceLock<Mutex<Vec<(Weak<Vec<Value>>, Option<i64>, Option<i64>)>>>;
static DEFERRED_SEQ_ITERS: OnceLock<Mutex<HashMap<usize, Value>>>;   // <-- usize, not Weak
```

The first four hold `Weak`s and prune expired entries, so they are merely O(n)-scanned and
address-compared. `DEFERRED_SEQ_ITERS` is different and worse:

1. **It leaks.** The key is `Arc::as_ptr(..) as usize` and the only removal is
   `seq_take_deferred_iter`. A deferred Seq that is dropped without ever being reified leaves its
   iterator `Value` in the map for the life of the process — along with everything a user
   `Iterator` instance closes over.
2. **The retained `Value` is invisible to the collector.** ADR-0001's Bacon-Rajan collector traces
   `ValueView::Seq(items)` (`src/value/value_gc.rs:139`) but knows nothing about this map. The
   iterator edge is an unregistered external strong reference. Not a soundness bug (external refs
   keep nodes alive, they do not falsely free them), but it is an untraceable retention path.
3. **It is an ABA hazard by inspection.** Every deferred Seq allocates an identically-shaped
   `Arc<Vec<Value>>` over an *empty* vec, so freed-and-reused allocations are plausible; a fresh
   Seq landing on a stale key's address would be seen as carrying that key's iterator. A 400-Seq
   allocate/free/re-allocate probe (`tmp/aba.raku`) did **not** reproduce it, so this is a
   by-inspection hazard, not a measured one. Recorded because moving the state into the value
   removes it for free rather than because it needs its own fix.

### 1.7 Why it matters now

rakudo's real `Test.rakumod` opens `is` with `if $got.defined { … $got eq $expected }`. Under
`MUTSU_REAL_TEST=1`, `t/is-lazy-io-lines.t` fails 2 of 3:

```
not ok 1 - .lines compares as its contents in `is`
# expected: 'A B C'
#      got: '(...)'
```

`.defined` gutted the value; the subsequent `eq` renders `LazyIoLines`'s placeholder gist
(`src/value/display.rs:478`). This is one of the remaining gaps in
`todo/deep/vendor-real-test-module.md`, and every `t/` file that hands a lazy `.lines` to the real
module hits it.

---

## 2. Decision

**Give `Seq` a real body. Reification fills that body in place and is idempotent; consumption is a
separate, explicit act performed only by the methods that steal the iterator. The five global
side tables and the `LazyIoLines` variant fold into the body.**

```rust
// src/value/seq_body.rs (new)

/// What a `Seq` still has to do to produce its elements.
pub(crate) enum SeqSource {
    /// Already reified at construction (`Value::seq(vec)`) — the common case.
    Reified,
    /// `Seq.new($iterator)`: pull `pull-one` until `IterationEnd`.
    Iterator(Value),
    /// `IO::Handle.lines` / `.words` — today's `ValueView::LazyIoLines`.
    IoLines { handle: Value, words: bool, kv: bool },
    /// The iterator was handed away by `.iterator`/`.list`/`.map`/… .
    /// A later *iteration* throws `X::Seq::Consumed`; a later *inspection*
    /// (`.raku` on a spent Seq) still answers.
    Taken,
}

pub(crate) struct SeqBody {
    /// Every decode generation; the last is live. A `Seq` transitions at most
    /// once (seed -> reified), so this holds at most two entries — the bounded
    /// degenerate case of ADR-0030 §2.2's graveyard, and it exists for the same
    /// reason: a `&Vec<Value>` handed out before the fill must stay valid.
    gens: SyncUnsafeCell<Vec<Box<Vec<Value>>>>,
    /// Source + flags. A `Mutex` (not a cell): this is genuinely written from
    /// several threads for `hyper`/`race`, and it is touched once per Seq
    /// lifecycle event, never per element read.
    state: Mutex<SeqState>,
}

struct SeqState {
    source: SeqSource,
    cached: bool,                              // was CACHED_SEQS
    lazy: bool,                                // was LAZY_SEQS
    hyper: Option<(Option<i64>, Option<i64>)>, // was HYPER_CONFIGS
}
```

`Value::Seq`, `Value::HyperSeq`, and `Value::RaceSeq` carry `Arc<SeqBody>` instead of
`Arc<Vec<Value>>`.

### 2.1 `Deref` is what keeps this a bounded change

```rust
impl std::ops::Deref for SeqBody {
    type Target = Vec<Value>;
    fn deref(&self) -> &Vec<Value> { self.live_generation() }
}
```

There are **330** `ValueView::Seq(..)` match sites in `src/`. Almost all of them bind the payload
and immediately treat it as a slice (`items.len()`, `items.to_vec()`, `&items[..]`, `items.iter()`).
With `ValueView::Seq(ArcRef<'a, SeqBody>)` and the `Deref` above, those sites **compile
unchanged**. This is ADR-0030 §3.2's argument applied to a second type, and it is the difference
between a ~120-site change and a ~450-site one.

`Deref` returning the live generation also means a read that arrives *before* reification sees the
empty seed, exactly as today — reification is triggered by the dispatch sites (§2.3), not by a
read. That is deliberate: a `Deref` that could run the VM would be a re-entrancy hazard at every
array read, and `Trace` must be able to walk the payload at a collect safepoint without
allocating (ADR-0030 §2.4).

### 2.2 Reify in place, and why it needs an interior-mutability primitive

```rust
impl SeqBody {
    /// rakudo's `.cache`: pull the source exactly once, store the elements in
    /// THIS body, and answer from them forever after. Idempotent.
    pub(crate) fn reify(&self, pull: impl FnOnce(&SeqSource) -> Result<Vec<Value>, RuntimeError>)
        -> Result<&Vec<Value>, RuntimeError>;

    /// rakudo's `.iterator`: hand the source away and mark the body `Taken`.
    /// Errors with `X::Seq::Consumed` if the source was already taken and the
    /// body was never reified.
    pub(crate) fn take_source(&self) -> Result<SeqSource, RuntimeError>;
}
```

`reify` writes under a shared `&SeqBody` that its caller keeps using — `Arc<T>` gives no `&mut`
(refcount > 1 by construction, since the whole point is that aliases observe the fill), and
`*const Self as *mut Self` is the exact UB ADR-0030 §1.1 diagnosed and removed. So `gens` sits
behind the **already-shipped** `SyncUnsafeCell` (`src/value/sync_cell.rs`), and a fill **pushes a
new `Box<Vec<Value>>`** rather than overwriting the seed, so any `&Vec<Value>` handed out earlier
stays valid.

Per ADR-0030 §6's rule — *"whichever primitive a call site reaches for, it must say why the other
one does not apply"* — [ADR-0013](0013-container-interior-mutability-cellvalue.md)'s
`gc_contents_mut` does not apply here for two reasons: `SeqBody` lives behind an `Arc`, not a `Gc`,
so there is no handle to route through; and the shape is a read-path fill under a borrow the caller
keeps using, which ADR-0013 §8 measured as UB under both Stacked and Tree Borrows.

The graveyard is far cheaper here than in ADR-0030: a `Seq` body reifies **at most once**, so it
holds at most two generations, and the seed is a zero-capacity `Vec`.

### 2.3 One consumption table, defaulting to *non*-consuming

The four disagreeing lists of §1.4 collapse into one function next to `SeqBody`:

```rust
/// True iff this method steals the Seq's iterator (rakudo: routes through
/// `.iterator`/`.list` rather than `.cache`). **The default is `false`** —
/// a method that merely needs the elements reifies and leaves the Seq usable.
/// The set is pinned by `t/seq-consumption-matrix.t`, which is generated from
/// a `raku` run so drift from rakudo is a test failure, not a code review.
pub(crate) fn seq_method_consumes(method: &str) -> bool
```

Every one of the ten sites in §1.5 becomes the same two lines: `reify` if the method needs
elements, then `take_source` **only** if `seq_method_consumes(method)`. `X::Seq::Consumed` is
raised by `take_source` on an already-taken, never-reified body — one place, instead of the two
guards (`methods_call_dispatch.rs:552-564`) and one atomic swap (`vm_helpers_lazy.rs:14`) that
raise it today.

### 2.4 `LazyIoLines` becomes a `SeqSource`, not a `Value` variant

`ValueView::LazyIoLines` exists only to say "these elements come from a filehandle, and I have not
read them yet" — which is `SeqSource::IoLines`. Folding it in is what actually fixes the headline
repro, and it deletes: the variant, its `LazyIoLinesBox` NaN-box kind, its `consumed: Arc<AtomicBool>`,
its `"(...)"` display special case (`display.rs:478`), its bespoke exemption list, and the name-keyed
`.cache` writeback band-aid (§1.3) — the band-aid becomes unnecessary rather than merely wider,
which is what makes rows 2 and 3 of the §1.3 table pass.

### 2.5 What does *not* change

- **`Value::seq(Vec<Value>)` keeps its signature** — 302 call sites untouched.
- **Element identity/equality/`Trace`** stay by-contents; `Trace` gains the retained generations
  and the `SeqSource`'s `Value`s (fixing §1.6-2 as a side effect).
- **`LazyList`** (gather, `map`/`grep` pipes, sequence specs) is untouched. See §6.

---

## 3. Options considered

| Option | Fixes §1.3 rows 2/3? | Fixes row 5? | Sound? | Blast radius | Verdict |
| --- | --- | --- | --- | --- | --- |
| **0.** Widen the name-keyed `.cache` writeback to every non-consuming method | ✗ | ✗ | ✓ | ~5 lines | Rejected — §3.1 |
| **1.** Transfer the pulled items into the old `Arc` after the fact | ✗ (UB) | ✓ | ✗ | small | Rejected — §3.2 |
| **2.** Store the reified items in a sixth global side table | ✓ | ✓ | ✓ | ~15 sites | Rejected — §3.3 |
| **3.** Reify eagerly at construction (delete laziness) | ✓ | ✓ | ✓ | small | Rejected — §3.4 |
| **4.** `ValueView::Seq(GcRef<SeqData>)` — a `Gc` body, not an `Arc` one | ✓ | ✓ | ✓ | as §2 + GC | Rejected — §3.5 |
| **5. `Arc<SeqBody>` with `Deref`, `SyncUnsafeCell` fill, split reify/consume** | ✓ | ✓ | ✓ | ~120 sites | **Recommended** |

### 3.1 Why not just widen the writeback (the cheapest thing that "fixes the ticket")

It is tempting: adding `.Str`/`.gist`/`.defined`/`.elems`/`.Bool` to the `method == "cache"` guard
at `vm_call_method_mut_ops.rs:667` makes the headline repro pass today. It is rejected because
rows 2 and 3 of §1.3 were measured specifically to test it: a writeback keyed on `target_name` in
the *current* frame's env cannot reach a Seq passed to a sub, and cannot reach a second alias. It
would also entrench a mechanism whose failure mode is silent (the caller keeps a stale value; no
diagnostic), and grow a fifth method list to disagree with the other four. Per CLAUDE.md's
gain/risk definition this is a band-aid whose "gain" is the appearance of progress.

### 3.2 Why not write the pulled items back into the existing `Arc<Vec<Value>>`

This is the ticket's own suggested shape ("transfer the pulled items to the old `Arc`"), and it is
the right *idea* — but `Arc<Vec<Value>>` contains no `UnsafeCell`, so obtaining a `*mut Vec<Value>`
from the live `&Vec<Value>` and writing through it is undefined behavior under both Stacked and
Tree Borrows. It is the identical construct ADR-0030 §1.1 found miscompiling under release
optimization in `ArrayData::sync_native_items`, where LLVM reused a cached load across the write.
Doing it again, knowingly, in the hottest container in the interpreter is not defensible. Fixing
it requires a cell — and a cell requires a struct to put the field in, which is option 5.

It also does not fix §1.3 row 2/3 for the `LazyIoLines` flavour, which has no element `Arc` at all.

### 3.3 Why not a sixth global side table holding the reified items

`REIFIED_SEQ_ITEMS: HashMap<usize, Arc<Vec<Value>>>` would let a reify be observed by every alias
without touching the `Value` representation. It fails on the read path: the 330
`ValueView::Seq(items)` sites read the payload `Arc` **directly**, and `peek.rs`'s decode
(`Kind::Seq => ValueView::Seq(arc_guard(bits))`) has nowhere to splice a side-table lookup that
outlives the `deref` call. Every one of those 330 sites would have to be rewritten to consult the
table — strictly more work than option 5, for a design that keeps the leak (§1.6-1), keeps the ABA
hazard (§1.6-3), and adds a mutex acquisition to the element read path.

### 3.4 Why not delete the laziness

Reifying at construction makes every one of these bugs vanish. It also makes `Seq.from-loop`
without a condition hang, breaks `$*ARGFILES.lines` streaming, and turns `(1..Inf).Seq` into a
non-terminating program. Non-starter; noted only because "materialize early and stop worrying" is
the shape mutsu has been drifting toward one exemption list at a time.

### 3.5 Why an `Arc` body rather than a `Gc` one

A `Gc<SeqData>` would put the body in the collector's graph and let `SeqSource`'s `Value` be traced
as an ordinary internal edge. But `Seq` is not currently a cycle participant, `Arc<Vec<Value>>` is
what all 330 read sites and the NaN-box `pack_arc`/`peek_arc` path already handle, and moving a
container into the `Gc` graph is an ADR-0001-scoped change with its own safepoint and
`gc_contents_mut` obligations. `Arc<SeqBody>` gets the whole fix with none of that; `Trace` still
walks the body through `ValueView::Seq` (§2.5), so the untraced-iterator retention of §1.6-2 is
fixed either way. Revisit only if a measured Seq-participating cycle appears.

---

## 4. Migration plan

Ordered so each phase is independently reviewable and CI-verifiable. **Phase 3 is the one that
closes the ticket** — phases 1-2 do not, because the headline repro is `LazyIoLines` (§1.2). Do not
stop after phase 2 and mark the ticket done.

| # | Step | Files | Notes |
| --- | --- | --- | --- |
| **1** | Land `t/seq-consumption-matrix.t`, the §1.4 oracle, with the currently-wrong rows marked `todo`. Land `t/seq-reify-preserves-aliases.t` (the §1.3 rows 1-3, 5). | `t/` only | Pins the target semantics *before* any mechanism moves. Turns "did we regress?" into a test rather than a judgment. |
| **2** | Introduce `SeqBody` / `SeqSource` / `SeqState` + `Deref`. Swap `Value::Seq`/`HyperSeq`/`RaceSeq` to `Arc<SeqBody>`. Fold `CACHED_SEQS` / `CONSUMED_SEQS` / `LAZY_SEQS` / `HYPER_CONFIGS` / `DEFERRED_SEQ_ITERS` into `state`; rewrite the ten `seq_*` free functions in `src/value/mod.rs` as `SeqBody` methods keeping their current names and signatures where possible. Update `Trace`/`drop_gc_edges`. | new `src/value/seq_body.rs`; `src/value/mod.rs`, `view.rs`, `value_gc.rs`, `nanbox/{decode,encode,peek,mod}.rs`; the 64 `Value::seq_arc` sites | Pure representation move, no semantic change. `Value::seq(Vec)` and the 330 `ValueView::Seq` read sites are untouched (§2.1). Most `seq_arc(Arc::new(v))` sites collapse to `Value::seq(v)`; the handful that re-wrap an existing `Arc` to preserve identity become `Value::seq_body(body.clone())`. |
| **3** | Split reify from consume. Add `SeqBody::reify` / `take_source` and `seq_method_consumes`. Rewrite the ten sites of §1.5 to call them. Delete `seq_deferred_method_keeps_lazy`, the inline `matches!` list, `consumed_methods`. Un-`todo` phase 1's rows. | `src/vm/{vm_coerce_concat_ops,vm_call_method_mut_ops,vm_native_dispatch,vm_call_method_compiled_interpret,vm_var_assign_set_local,vm_for_loop_dispatch}.rs`, `src/runtime/methods_call_dispatch.rs`, `src/builtins/methods_0arg/dispatch_core_math.rs` | **Fixes §1.3 row 5.** Expect roast churn: constructs that silently returned `()` now return elements, and `.iterator` starts consuming (§1.4's inverted row). |
| **4** | Fold `LazyIoLines` into `SeqSource::IoLines`. Delete the variant, `LazyIoLinesBox`, the `consumed` `AtomicBool`, `force_if_lazy_io_lines`, the `display.rs:478` `"(...)"` case, and the name-keyed `.cache` writeback. | `src/value/{view,display,serde_support,nanbox/{peek,boxes}}.rs`, `src/vm/{vm_helpers_lazy,vm_call_method_mut_ops,vm_for_loop_dispatch,vm_data_ops}.rs`, `src/runtime/{resolution_lazy,builtins_collection_listops}.rs`, `src/builtins/{methods_0arg/collection,functions/dispatch_1arg}.rs` | **Closes the ticket.** `MUTSU_REAL_TEST=1 prove t/is-lazy-io-lines.t` must go 3/3. |
| **5** | Add `src/value/seq_body_shapes.rs`, a Miri probe module modelled on `src/value/native_cache_shapes.rs`, and extend `ci.yml`'s Miri filter to reach it (ADR-0030 §5 records that the filter is a substring match on `--lib gc::` and silently selects nothing new otherwise). | new file; `.github/workflows/ci.yml` | Probes: (a) a `&Vec<Value>` taken before a `reify`, used after it; (b) the retired generation still reads as empty while a fresh borrow reads the elements; (c) `SeqBody: Sync` and `Arc<SeqBody>: Send + Sync`; (d) two `Arc` clones both observe one `reify`. |

### Verification

```sh
raku tmp/matrix.raku; timeout 60 target/debug/mutsu tmp/matrix.raku   # must agree row for row
raku tmp/probe2.raku; timeout 60 target/debug/mutsu tmp/probe2.raku   # rows 1-5 must agree
MUTSU_REAL_TEST=1 timeout 60 target/debug/mutsu t/is-lazy-io-lines.t  # 3/3
prove -e target/debug/mutsu t/seq-*.t t/lazy-seq-*.t t/try-sink-semantics.t
```

---

## 5. Risks

- **Phase 3 makes mutsu stricter, deliberately.** `.iterator` starts consuming, and `.List` after a
  consuming method starts throwing where mutsu was lax. Whitelisted roast files that relied on
  mutsu's permissiveness will go red. That is the safety net working (CLAUDE.md's gain/risk
  definition), but it means **phase 3 needs a full `make roast` in CI, not just `make test`**, and
  the PR should expect a fix-forward round.
- **Phase 3 may also make some things *looser* in a way that hides a bug.** Methods that used to
  throw `X::Seq::Consumed` will now succeed. A test that asserted the throw is a real signal — check
  it against `raku` before "fixing" the test.
- **The §1.5 site list may be incomplete.** It was assembled by grepping the ten `seq_*` helpers; a
  site that reifies by some other route (e.g. a `value_to_list` call on a Seq) would be missed. The
  phase-1 oracle test is the mitigation: a missed site shows up as a row that still disagrees.
- **`Deref` on a not-yet-reified body returns an empty vec.** Any read site that today *implicitly*
  relies on the reify having been forced by an earlier dispatch guard keeps working only if that
  guard is still there. Phase 3 rewrites the guards; the risk is a guard removed a beat too early.
  Mitigation: phases 2 and 3 are separate PRs, and phase 2 changes no semantics at all.
- **`HyperSeq`/`RaceSeq` share the payload type** and therefore the migration. `hyper_config_get`'s
  `Weak`-scan semantics must be preserved exactly when it moves into `SeqState`, or `:batch`/`:degree`
  silently revert to defaults — a *performance* regression with no test signal. Pin it in phase 2.
- **Not a perf change either way.** The element read path gains one `Deref` hop through
  `SeqBody`; it loses a `Mutex` lock plus an O(n) `Weak` scan on every `seq_is_cached` /
  `seq_is_consumed` / `seq_is_lazy` probe, which today runs on the *dispatch* path of every method
  call on a Seq. Expect neutral-to-better; do not sell it as an optimization without a bench row.

---

## 6. Scope: what this ADR deliberately does not decide

The ticket's second half ("Residual try-cell divergences", P4/P5/P12/P13/P18/Q5/Q6/Q9/Q11/Q14/R6/R7)
is a **different** defect: mutsu forces a `map`-produced `LazyList` at the assignment/call boundary,
where raku keeps it lazy until something consumes it, so a `die` inside the map body fires inside a
`try` that raku has already let the value escape from. That is about **where** forcing happens, in
`LazyList`, not about **what** forcing does to a `Seq`. It is not fixed by this ADR and should not
be bundled into it.

**Follow-up (2026-08-22):** that scoped-out work is now
[ADR-0058](0058-map-grep-produce-a-deferred-seq.md), which also corrects the
mechanism named above: the cells do *not* go through `LazyList` at all — a
finite `(1..3).map(...)` is evaluated **eagerly at the `.map` call**, so there is
no deferred value whose force could be moved. ADR-0058's decision is to give
`SeqSource` a `MapGrep` variant and let this ADR's own reify/consume split do the
deferring.

The relationship is worth recording, though: those cells are hard to fix today partly because
deferring the force means the value gets touched later and *by more consumers*, and every extra
touch is currently a chance to hit the destroy-on-materialize bug. Landing this ADR removes that
coupling, so the laziness work becomes tractable on its own terms. When it is attempted, the
ticket's own warning stands: it makes mutsu stricter in every one of those cells and needs a full
roast sweep. `t/try-sink-semantics.t` pins the cells that already match.

---

## 7. Consequences

- **`Seq` gets rakudo's two primitives instead of mutsu's one.** "Reify" (idempotent, in place,
  `.cache`) and "consume" (steal the source, once, `.iterator`) become separate operations, and the
  default for a method that merely reads elements flips from consuming to non-consuming — matching
  the measured oracle in §1.4.
- **Four disagreeing method lists and ten reify sites collapse to one list and one pair of
  operations.** This is the maintainability payout; the correctness payout is that a fifth list
  cannot drift into existence.
- **Three representations of `Seq` become two** (`Seq` and `LazyList`), and `ValueView` loses a
  variant.
- **Five global side tables and a name-keyed env writeback are deleted.** With them go a real leak,
  an untraced retention path, and an ABA hazard (§1.6).
- **mutsu's second interior-mutability primitive gets its second user**, which is the point of
  ADR-0030 §6's rule existing: `SyncUnsafeCell` is now demonstrably the general answer for
  "read-path fill under a shared borrow", not a one-off for `ArrayData`.
- **If rejected**: the headline repro stays broken, `MUTSU_REAL_TEST=1 t/is-lazy-io-lines.t` stays
  at 1/3, and every future lazy-`Seq` consumer adds an entry to one of the four exemption lists —
  which is precisely how the current state was reached.

---

## 7.1 Outcome (2026-08-19/20)

Implemented in one PR: phases 1-4 landed together (phase 5, the Miri probe module, was **not**
done — see below), matching open question 1's recommendation. `SeqBody`/`SeqSource`/`SeqState`
behind a `Mutex` (open question 2's recommendation, unchanged — no bench pressure surfaced).

The headline repro (§1.1) is fixed and verified against the ADR's own reproduction script
line-for-line: `$b.defined; $b.Str` on a fresh `IO::Handle.lines` Seq now matches raku exactly. The
§1.3 alias-preservation rows (1-3, 5) and the §1.4 consumption matrix are pinned by
`t/seq-reify-preserves-aliases.t` and `t/seq-consumption-matrix.t` respectively — both written
*after* the mechanism (not before, as phase 1 of the plan suggested) and cross-checked against a
real `raku` run of the same files, not just against the design's predictions.

**Open question 3 was answered differently than recommended, because the recommendation was wrong.**
Measuring `raku` directly (not just the `.iterator`-vs-Reified corner the ADR's own §1.4 oracle
sampled) showed rakudo's `Seq` is single-use by default *even when built from a fully-known literal
list* — `my $s = (1,2,3).Seq; $s.List; $s.List` throws `X::Seq::Consumed` on the second call in real
raku. "`.iterator` on an already-reified Seq should serve" turned out to generalize to "every
`seq_method_consumes` entry steals a body's first touch unless an earlier NON-consuming touch or an
explicit `.cache` already made it durable" — a `retained` flag on `SeqState`, set only by
`SeqBody::reify` (never by construction). This is *stricter* than the ADR's §2.5 claim that
`Value::seq` keeps unchanged, always-servable behavior; that claim was the accidental result of the
old side-table design, not a deliberate rakudo-matching choice, and roast (`roast/S32-list/seq.t`'s
"methods still throw when Seq is NOT cached" subtest) confirmed the stricter reading is the correct
one. See `SeqBody::take`'s doc comment for the full reasoning and measurements.

**One residual, explicitly-accepted gap: mutsu's parser cannot distinguish `@$s` (sigil
array-context deref of a Scalar — never consumes in raku, even over a deferred source) from an
explicit `.list()` method call (consumes) — both desugar to the identical method-name string
`"list"`.** Two pinned local tests exercise opposite sides of this
(`t/seq-array-context-reiterate.t` for `@$s`, `t/io-handle-lines-words-seq.t` for explicit
`.list`), so `reify_or_consume_seq_target`'s `"list"` branch (`src/vm/vm_helpers_lazy.rs`) carries a
documented compromise: steal a genuinely deferred source, but never steal an already-`Reified` body.
The one case this leaves wrong relative to raku — explicit `.list()` on an already-`Reified` body
staying reusable — is pinned as a KNOWN GAP subtest in `t/seq-consumption-matrix.t`. A real fix
needs the parser to emit a distinct method name for the two call shapes; out of scope here.

**Phase 5 (the Miri probe module, `src/value/seq_body_shapes.rs`) was not done** in this PR —
deferred to `todo/tickets/adr0034-phase5-seq-body-miri-probes.md` rather than extending the session
further. Functional correctness is covered extensively by `t/` and roast; phase 5 is soundness-probe
infrastructure, not a functional gap. **Done in a follow-up PR** (see
`news/2026-08/adr0034-phase5-seq-body-miri-probes.md`): `src/value/seq_body_shapes.rs` probes the
`SeqBody` generation graveyard the same way `native_cache_shapes.rs` probes `NativeBacking`'s, and
CI's `miri` job runs it via an explicit `--lib value::seq_body_shapes` invocation (`ci.yml`'s filter
is a substring match on `gc::`, which does not select either module).

Two bugs surfaced only by end-to-end testing, neither anticipated by the design (§1.5's site list
was, as risk §5 predicted, incomplete):

- The `...` sequence operator's own generator step can feed a just-produced `Seq` element (e.g.
  `*.reverse`'s result) back into itself to compute the *next* element — an internal consuming touch
  on a value the user's `$seq[N]` read also aliases. Fixed by reifying (not consuming) each
  generator step's result before storing it (`src/runtime/sequence.rs`).
- `Value::eqv`'s `(Seq, Seq)` arm is a pure, interpreter-free comparison that reads a body's elements
  via `Deref` directly — correct for the old "Seq is always already-materialized" invariant, wrong
  now that a `ValueView::Seq` can be a genuinely deferred, not-yet-pulled body (even one already
  `.cache`d, since `.cache` itself is lazy). Fixed by reifying/consuming both `eqv` operands in the
  `exec_eqv_op` VM handler, which has the `&mut Interpreter` access `Value::eqv` structurally cannot
  (`src/vm/vm_comparison_order_ops.rs`). Surfaced by `roast/S16-io/words.t`'s
  `is-eqv words(), <...>.Seq` (`Test::Util`'s `is-eqv` explicitly `.cache`s both sides before
  comparing).

## 8. Open questions for the deciders

1. **Should phase 4 (folding `LazyIoLines`) be in the same PR as phase 3?** They are separable, but
   phase 3 alone leaves the ticket open and leaves two consumption mechanisms live simultaneously —
   arguably the worst intermediate state. **Recommendation: same PR as phase 3, or immediately
   after with nothing else landing in between.**
2. **Is `SeqState` behind a `Mutex` right, or should it be atomics + a cell?** The `Mutex` is
   touched once per lifecycle event, not per element read, so contention should be nil — but
   `hyper`/`race` genuinely write it from worker threads, and a `Mutex` in a `Value` payload is a
   re-entrancy shape ADR-0013 §3 was wary of. **Recommendation: `Mutex`, and revisit only if a
   `hyper`/`race` bench shows it.**
3. **Should `.iterator` on an already-*reified* Seq succeed (serving the cached elements) or throw?**
   rakudo serves them — that is why `.cache` then `.List` works. The design assumes serve.
   **Recommendation: serve; pin it as an explicit row in `t/seq-consumption-matrix.t`.**

---

*This ADR is `Accepted` and implemented — see §7.1 for the outcome, including where implementation
diverged from the design (open question 3). If the mechanism judgment changes again, supersede this
ADR rather than rewriting it.*
