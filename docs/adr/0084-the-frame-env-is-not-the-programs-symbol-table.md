# ADR-0084: The per-frame `Env` is not the program's symbol table

- Status: Proposed
- Date: 2026-09-09
- Related: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-addressed lexical capture and env synchronization),
  [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (a mainline routine
  carries the lexical store of its compilation unit),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (replace by-name
  lookup with an owning lexical scope), and
  [ADR-0081](0081-compunit-scoped-module-import-aliases.md) (a unit module's
  imported aliases are scoped to its compilation unit)
- Addresses: GitHub issues
  [#7667](https://github.com/tokuhirom/mutsu/issues/7667) (the measurements
  below), [#7787](https://github.com/tokuhirom/mutsu/issues/7787) (a module's
  `constant`s and enum values leak by this route), and the still-open
  divergence (b) of [#7555](https://github.com/tokuhirom/mutsu/issues/7555)
- Tracked by: [#7817](https://github.com/tokuhirom/mutsu/issues/7817)

## 1. Context

### 1.1 What is measured

`Env` is mutsu's per-frame lexical environment: an `Arc<FxHashMap<Symbol,
Value>>` that call frames clone, closures capture, and (until #7796) thread
clones deep-copied. It is copy-on-write, so a write to a *shared* env copies the
whole map.

Dumping the keys of one frame env at the moment it is deep-copied, in a program
that has done nothing but `use Cro::HTTP2::RequestParser` and build one
`Promise(supply { whenever … })` — **403 entries**:

| kind | count |
| --- | --- |
| `__mutsu_callable_id::Pkg::name` markers | **128** |
| qualified type/package names (`Cro::HTTP2::Frame`, `Cro::BodyParser`, …) | **159** |
| leading-capital type/enum names (`Any`, `PROTOCOL_ERROR`, `CANCEL`, …) | 51 |
| `__mutsu_constant_var::` markers | 27 |
| dynamics (`$*x`) | 20 |
| bare lowercase subs/terms | 8 |
| `__mutsu_type::` markers | 6 |
| **actual sigilled lexical variables** | **2** |

Two entries out of 403 are lexical variables. Deeper frames in the same program
reach ~790 entries with the same composition. The equivalent env in a program
with no `use` is 34 entries.

### 1.2 What it costs

The env is the structure everything copies, so its size is a multiplier on
several unrelated hot paths:

- **Frame setup.** `call_sub_value` builds a call frame's env with a
  `clone` → `insert` → `clone` → `insert` sequence; each insert taken while the
  map is shared copies all ~790 entries. Measured: 22 deep copies totalling
  17,453 entries per `Promise(supply { whenever … })`, ~1.95M instructions,
  roughly 0.5 ms.
- **Closure capture.** A closure's captured env is a flat snapshot of everything
  in scope, so creating one is O(program), not O(free variables) — even though
  the compiler already computes `free_var_syms`.
- **Thread spawn.** Fixed for the *program tables* by #7796, but the env itself
  is still copied per spawn lineage.
- **Identical code, different cost.** The same six lines of Raku cost 0.21 ms in
  a bare program and 1.17 ms after `use Cro::HTTP2::RequestParser`, with
  **identical opcode counts** — the whole difference is native-side copying that
  scales with how many names are in scope.

Per-site attribution for the 22 copies (entries copied per iteration):

```
  2384  resolution_call_sub.rs:784   __mutsu_callable_id insert after block_sub shares the env
  2383  resolution_call_sub.rs:769   &?BLOCK insert after block_arc shares the env
  2382  native_supply_methods.rs:284 sub_with_env_key
  2379  resolution_call_sub.rs:1014  persist_closure_env rebuild
  1584  types/binding_signature.rs:627
  1577  env.rs:1477
   796…788  methods_mut_substr_buf, methods_mut_dispatch, vm_var_assign_set_local,
            vm_register_ops, resolution_call_sub.rs:739 / :674
```

### 1.3 Why this is not a micro-optimization problem

The obvious local fix — reorder the `clone`/`insert` pairs so one deep copy
serves several inserts — was tried and **rejected on the spot**: the position of
the `__mutsu_callable_id` insert is load-bearing. `call_sub_value` decides
whether a non-local `return` propagates with

```rust
let has_target = e.return_target_callable_id().is_some()
    || data.env.contains_key("__mutsu_callable_id");
```

so moving that insert earlier puts the key into `block_sub`'s captured env, and
`block_sub` is reachable and callable as `&?BLOCK`. That changes `return`
semantics inside `&?BLOCK()` to buy ~0.09 ms. Under this repo's own definition
(CLAUDE.md, "What gain and risk actually mean") that is a risk, not a gain.

The remaining sites are the same shape: each is *individually* justified, and
each is expensive only because the map it copies is 400-790 entries of things
that are not lexical variables.

### 1.4 The correctness half

This is not only a performance question. The same storage choice is the root of
two open correctness divergences:

- #7555 divergence (b): `run_modules.rs`'s `new_types` → `package_type_aliases`
  write deliberately aliases every class a module load registered into the
  *importer's* package, to compensate for weaker lexical scoping. Rakudo does
  not make those names visible.
- #7787: a module's file-scope `constant`s and enum values reach the importer by
  the same route.

So the entries that dominate the copy cost are, in part, entries that should not
be visible at all.

## 2. Decision

**Proposed.** The per-frame `Env` holds *lexical variables and dynamics*. Type
and package names, a compunit's constants and enum values, and internal
per-callable bookkeeping move to package-/compunit-keyed side tables that
bareword and routine resolution consult directly, and that frames neither clone
nor capture.

mutsu already has the receiving structures — `package_type_aliases`,
`module_scope_lexicals`, `package_lexicals`, `unit_lexicals` — and, since #7796,
they are `Arc` copy-on-write, so moving entries into them makes those entries
free to carry across frames and spawns rather than merely cheaper.

Three separable groups, in increasing order of blast radius:

1. **`__mutsu_callable_id::Pkg::name` markers** (128 of 403). Internal
   registration markers, not user-visible names. They are deliberately *lexical*
   (`resolution_eval.rs` snapshots and restores them per block), so the move
   needs a scope-aware side table rather than a flat one, but nothing about them
   is a Raku-level binding.
2. **Type/package names and enum values** (210 of 403). This is the group the two
   correctness issues above are already about, so the scoping work and the
   storage move are the same work.
3. **`__mutsu_constant_var::` / `__mutsu_type::` markers** (33 of 403). Same
   character as group 1.

## 3. Invariants

Whatever the storage, these must continue to hold:

- **Lexical shadowing.** An inner `my` shadows an outer binding of the same name,
  and a block's declarations do not outlive it.
- **Rakudo visibility.** A transitively-`use`d module's names are not visible in
  the importer (#7555, #7787, ADR-0081); a directly-`use`d module's *exported*
  names are.
- **Non-local return targeting.** Whatever replaces
  `data.env.contains_key("__mutsu_callable_id")` must answer the same question:
  does this code object name a routine a `return` can target.
- **Closure capture semantics.** A closure sees the bindings in scope at its
  creation, including later mutations through shared cells (ADR-0025,
  ADR-0039) — the storage move must not turn a shared cell into a snapshot.
- **Thread-clone isolation.** A spawned thread sees the parent's declarations and
  its own do not leak back (#7796, `t/thread-clone-program-table-isolation.t`).

## 4. Acceptance

- `env_deep_copy_entries` (the `MUTSU_VM_STATS` counter added in #7800) per
  `Promise(supply { whenever … })` falls from 17,453 toward the bare-program
  figure of 749.
- The bare-vs-loaded ratio for identical code (0.21 ms vs 1.17 ms today) closes:
  a program's frame costs should not scale with how many modules it has loaded.
- `make test` and `make roast` stay green, and the battery gate does not regress.
- #7787's and #7555 divergence (b)'s visibility tables match rakudo.

## 5. Alternatives rejected

- **Reorder the `clone`/`insert` pairs.** ~18% of the copy cost (~0.09 ms) and
  it changes `&?BLOCK()`'s `return` semantics; see §1.3.
- **Make frame envs scoped overlays everywhere** (the `Env::scoped_child` shape
  ADR-0039 and #7800 use for speculative windows). Sound for a window that is
  discarded, but `Env::iter`/`keys`/`values`/`len` are overlay-only, so any
  long-lived capture — a closure's env, `&?BLOCK`, a thread clone — must
  `flattened()` first, which reintroduces the O(env) copy at exactly the sites
  that matter. It would become viable *after* this ADR, not instead of it.
- **A persistent (HAMT) map for `Env`,** making clone-then-insert O(log n). It
  treats the symptom, keeps the symbol tables in the frame, and leaves the two
  correctness divergences untouched.
- **Do nothing.** Defensible on the perf axis alone — the five merged fixes under
  #7667 already took a HTTP/2 HEADERS frame from 23.4 ms to 10.3 ms — but the
  correctness divergences are filed and open regardless, and they need this same
  work.

## 6. What this does *not* claim

It does **not** claim to stabilize `cro-http`'s `t/http2-request-parser.rakutest`
under `MUTSU_REAL_TEST=1`, which is what #7667 was opened for. That test loses a
race whose losing side (`DATA frame → body-blob → one ok`) costs ~3.1 ms against
a winning side of ~0.25 ms, and needs to come under about 1 ms. Removing *every*
env deep copy on that path is worth about 0.5 ms. This ADR is worth doing for the
correctness divergences and for the general "cost scales with program size"
property; the race needs that and more.

## 7. Implementation status

Not started, tracked by
[#7817](https://github.com/tokuhirom/mutsu/issues/7817). Nothing here has been
implemented; the measurements in §1 are from
`MUTSU_VM_STATS`, a `#[track_caller]` dump in `Env::cow_mut`, and `callgrind`,
all reproducible from a plain `Promise(supply { whenever … })` loop with and
without `use Cro::HTTP2::RequestParser`.
