# bench-ctor: construction stopped re-deriving names it already knew

`todo/perf/bench-ctor-construction-parity.md` round 6. The two leads round 5
left open inside `dispatch_bless` turned out to be the small half of the
finding. Profiling the whole benchmark with **callgrind** (this container has
no `perf`, and callgrind's instruction counts are deterministic, which is what
a perf iteration wants anyway) put `std::thread::local::LocalKey<T>::with` at
**9.2% of the entire run**, and its dominant caller was `Symbol::intern`:
**732,232 calls for 5000 constructions — 146 name interns per constructed
object**, each a thread-local round trip plus a string hash and a `memcmp`.
Almost all of them re-derived a `Symbol` from a name the caller already had,
or had computed once per class already.

The second finding is next to it: **every instance death cloned its whole
attribute map into a DESTROY queue** that, in a program with no `DESTROY`
anywhere, was walked and thrown away — a 21-entry `AttrMap` clone, a sharded
mutex, and an MRO walk interning two names per level, per constructed object.

Whole-bench instruction count, `benchmarks/bench-ctor.raku` (5000
constructions, release, `taskset`-pinned, callgrind): **1,715,336,347 →
1,480,264,577, −13.7%** (343k → 296k instructions per construction). An
interleaved same-session wall-clock A/B (release builds of `main` and of this
change, alternating, `taskset -c 2`, best of 9) reads **0.302s → 0.225s,
−25%** — larger than the instruction delta because much of what went away was
cache-unfriendly (thread-local + hash + `memcmp` round trips, and a 21-entry
`AttrMap` clone per constructed object). Wall-clock confirmation belongs to the
bench CI (`bench-history.tsv` on `bench-data`), per the ticket's measurement
notes.

## What changed

**`NativeCtorPlan` learned the two things `dispatch_bless` re-derived per
construction** — the ticket's own round-5 leads:

- `attr_index`: attribute name -> index. The named-argument override loop
  answered "does this argument name a declared attribute?" with a linear
  `class_attrs.iter().position(...)` scan *per argument* — on the
  `Zef::Distribution` shape that is 7 args x 21 attributes of `memcmp` per
  construction. Now one hash probe per argument, resolved once and reused by
  both the seed loop and the override loop. First-wins, matching the scan it
  replaces (an MRO can collect two same-named attributes).
- `attr_seeds`: the no-initializer seed value of each attribute, as a small
  `AttrSeed` enum. Deriving it meant a `type_constraints` lookup, a
  `nominal_type_object_name_for_constraint` walk and a `Symbol::intern` of the
  resulting type name for *every unfilled attribute of every construction* —
  16 interns per construction here. It is pure class shape, so it is computed
  once per class alongside the rest of the plan.

**A process-global "some user DESTROY exists" latch** (`ANY_DESTROY_DECLARED`
in `src/value/mod.rs`). Monotonic and armed from the two places a user
`DESTROY` can appear — `Registry::reindex_user_method_name` (the single
reverse-index hook every class-side method mutator calls: class bodies,
`augment`, `.^add_method`) and role registration (a role submethod `DESTROY`
is dispatched straight off `RoleDef::methods`, never through the class method
table). `InstanceAttrs::finalize_destroy` reads it at **drop** time, not at
construction, so a `DESTROY` installed later still fires for everything that
dies after it; it is read *after* the `live_instance_refcounts` bookkeeping, so
skipping the queue cannot leak refcount entries. Pinned by
`t/destroy-latch-late-registration.t`.

**Symbol-keyed MRO probes for method presence.** `has_user_method` walked the
MRO calling `Registry::user_method_overloads(cn.as_str(), name)`, which
re-interned *both* names at every level and then **cloned the whole
`Vec<MethodDef>`** — a ~300-byte struct with `String`s, `Arc`s and `Vec`s —
purely to read one boolean off it. That single function was 1.6% of the bench.
It now asks the new `Registry::user_method_public_presence(owner, name)`, which
allocates nothing; the MRO entries are already `Symbol`s and the method name is
interned once for the whole walk. `has_public_accessor` and
`resolve_user_method_or_accessor` got the same treatment through new
`accessor_is_public_sym` / `user_method_local_role_presence_sym`.

**`Symbol::intern("Any")` -> `crate::symbol::wk::any()`** across the tree (115
call sites). The well-known-symbol accessor already existed for exactly this;
`Any` is the single most-interned name in an OO program (it is every untyped
attribute's seed and every routine's fresh topic).

**`AttrReadGuard::drop` no longer consults the deferred-write queue** unless
something is actually deferred. Deferral is a rare self-deadlock escape hatch,
but every attribute read paid a second thread-local access, a `RefCell` borrow
and a `Vec::retain` to discover that — ~40 reads per construction. A relaxed
load of the new global `PENDING_CELL_WRITE_COUNT` answers it in a couple of
instructions (global rather than per-thread on purpose: over-reporting only
makes this thread run the correct, empty, scan).

**A dedicated `__mutsu_sigilless_readonly::` latch** (round 5's third open
item). `closure_meta_keys_possible()` lumps four unrelated key families
together, so creating a `state` variable armed the readonly probe too — and
that probe runs on *every* whole-variable assignment (`OpCode::CheckReadOnly`),
building a `format!` key and hashing it into the env for a key the program
never created. `sigilless_readonly_keys_possible()` is the narrower gate, armed
from the same `note_env_key` chokepoint every creation site flows through.

**Interning hoists in method dispatch.** `call_compiled_method{,_fast}`
interned the owner class name twice per call (once for `::?CLASS`, once for the
routine-frame push) and the non-fast path built `String`s for the fixed
`"?CLASS"` / `"?ROLE"` env keys instead of using the well-known symbols the
fast path already uses. An attributive parameter bind interned its attribute
name twice and allocated a `String` for the cell insert; it now interns once
and inserts by `Symbol`.

## Still open

The remaining `Symbol::intern` traffic is concentrated in three places, each
needing its own surgery: `native_lever_a_user_override` (2 interns per native
method dispatch — `value_type_name` returns a `&'static str`, so a
pointer-keyed memo or a `value_type_sym` would do it),
`get_env_with_main_alias_inner` (name-keyed env reads, ~16 per construction —
wants symbol-threaded callers), and the routine-frame push's
`lexical_package` / `method_name` (wants pre-interned symbols on `MethodDef`,
which is 18 struct-literal construction sites). Together they are ~3% of the
bench.
