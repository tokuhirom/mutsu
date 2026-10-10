# ADR-12529: A frame resolves names through its lexical outer, not its caller — separating the static link from the dynamic link

- **Status**: Proposed (2026-10-10)
- **Date**: 2026-10-10
- **Related**: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (locals are slot-addressed; escaping mutable lexicals are cells — this ADR
  builds on it and does not revisit it),
  [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (a named sub resolves
  mainline free variables through unit-lexical cells, not the ambient env —
  the same move, for one case),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (`@`/`%` lexicals
  resolve by slot, not by name),
  [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md)
  (Proposed: the frame `Env` is not the program's symbol table — phase 1 below
  is its completion),
  [ADR-0086](0086-builtin-dynamics-are-not-closure-capture-material.md),
  [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md),
  [ADR-0094](0094-closure-capture-kept-set-is-not-narrowed.md),
  [ADR-9170](9170-closure-capture-shares-system-name-layers.md) (the capture
  machinery this ADR retires),
  [ADR-0035](0035-method-calls-observe-caller-frames.md) (caller-frame
  observation — what the dynamic link must keep serving),
  [ADR-0010](0010-cross-thread-lexical-sharing-scope.md) (thread sharing),
  [docs/vm-single-store.md](../vm-single-store.md) (`locals` as the single
  authority, `env` as a derived view)
- **Addresses**: [#12529](https://github.com/tokuhirom/mutsu/issues/12529)
  (carrier), [#12520](https://github.com/tokuhirom/mutsu/issues/12520)
  (FunctionalParsers `t/08` is 4.9x rakudo),
  [#12519](https://github.com/tokuhirom/mutsu/issues/12519) (closures capture
  the caller's dynamic env)

## 1. Context

### 1.1 Two links, one chain

A Raku frame has two parents. Its **outer** is the frame of the code that
lexically encloses it — fixed when the closure was taken. Its **caller** is the
frame that invoked it. Lexical names (`my`, parameters, `sub` declarations,
which are `my &name`) are looked up through the outer chain only; dynamic
variables (`$*x`) and `CALLER::` go through the caller chain. MoarVM resolves a
lexical to `(outer depth, index)` at compile time, so a lexical read is an
indexed load and taking a closure binds one outer-frame pointer.

mutsu has the first half of this. Since ADR-0018, a compiled lexical lives in a
slot of the running frame (`locals`), escaping mutable lexicals are shared
`ContainerRef` cells, and a closure's free lexicals are upvalues. But the frame
still carries a **name-keyed `Env`**, and that `Env` has only one parent:

```
callee frame env = Env::scoped_child(<caller's env>)          // every call
closure frame    = overlay -> caller chain -> GLOBAL_BASE -> capture   (ADR-0092)
closure capture  = Env::layered_capture over the creating chain   (ADR-9170)
return           = callee overlay merged back into the caller env
```

`scoped_child(caller)` is installed by every call path:
`vm_call_named_inner.rs`, `vm_closure_dispatch.rs`, `vm_call_fast.rs`,
`vm_call_light.rs`, `vm_call_light_typed.rs`, `vm_method_dispatch.rs`. So the
`Env` chain *is* the caller chain, and everything that is not slot-addressed
resolves dynamically:

- **routine names** — `sequence(&p1, &p2)` is resolved by name per call
  (`dispatch_func_call_inner` → `resolve_function_with_types`,
  `has_multi_function` → `Registry::any_candidate_key_of`);
- **`&name` code variables** — `resolve_amp_var_for` → `Env::resolve_code_var`
  walks the env chain (its own doc comment calls the caller hit "dynamic
  scoping", and `declared_scope_amp_var_for` exists to pre-empt one case of it);
- **type and package names, constants, `__mutsu_*` metadata** — the kept set
  ADR-0094 declined to narrow;
- **anything a reflective lookup names** (`EVAL`, `::('$x')`).

A closure created in a callee then snapshots that dynamic chain (ADR-9170 copies
the narrow top tiers and layers the wide ones), which is #12519.

### 1.2 What it costs, measured

`main` @ `75e5ba07`, profiling build (release opt + debuginfo), 4-core container.
Full numbers in the #12520 comment.

| | mutsu | rakudo | ratio |
| --- | ---: | ---: | ---: |
| FunctionalParsers `t/08-ebnf-parsing.rakutest` | 20.1 s | 4.1 s | 4.9x |
| one parse of its `$ebnfCode13` block | 1.23 s | 0.25 s | 4.9x |

It is a constant per-call factor, not a complexity problem: scaling the grammar
from 1 to 32 rules, mutsu grows linearly (0.21 → 5.91 s) while rakudo grows
superlinearly (0.10 → 2.52 s), so the ratio falls from ~5x to 2.3x as the input
grows. FunctionalParsers is a combinator library — every parser is
`-> @x { ... }` and every combinator is a sub returning one — so per-call and
per-closure overhead is the whole workload.

Callgrind, one parse (~4.2G Ir of a 5.28G run; inclusive, rows overlap
slightly):

| mechanism | Ir | share of parse |
| --- | ---: | ---: |
| closure creation: `capture_closure_env` → `Env::layered_capture` (~1.0M hash inserts) | 770M | 18% |
| routine-name resolution per call (`resolve_function_with_types`, `resolve_function`, `has_multi_function`; 2.2M `Symbol::as_str` in candidate-key scans) | ~580M | 14% |
| `&p` / `&!pGExpr` lookup by name (`resolve_amp_var_for`, `resolve_code_var_scoped`) | 355M | 8% |
| per-frame env flatten (`flatten_scoped_env`, `Env::flattened_for_frame`) | 311M | 7% |
| env HashMap traffic (`insert_sym`, `Tier::insert`, `get_sym_with_fallback` at ~425 Ir per lookup) | ~600M | 14% |

Per-operation wall clock (same build, one run each, indicative only):

| | mutsu | rakudo |
| --- | ---: | ---: |
| call a sub `g($x)` | 1.58 µs | 0.79 µs |
| a sub that returns `-> $y { $x }` | 5.18 µs | 0.69 µs |
| call the closure it returned | 3.04 µs | 0.31 µs |
| `alt(symbol('a'), symbol('b'), symbol('c'))(@a)` | 91 µs | 4.1 µs |

### 1.3 What it gets wrong

The dynamic chain is also visible. A callee can read its caller's lexicals by
name:

```raku
sub g() { (try EVAL q[$secret]) // 'not visible' }
sub h() { my $secret = 'caller lexical'; g() }
say "g-from-h: " ~ h();
my &mk = -> { -> { (try EVAL q[$hidden]) // 'not visible' } };
sub k() { my $hidden = 'caller lexical'; mk() }
say "closure-from-k: " ~ k()();
```

| | `g-from-h` | `closure-from-k` |
| --- | --- | --- |
| mutsu (`75e5ba07`) | `caller lexical` | `caller lexical` |
| raku | `not visible` | `not visible` |

The same leak has produced a line of closed tickets, each fixed by adding a
store that is consulted *before* the env so the dynamic hit is not reached:
ADR-0024 (mainline free variables), `declared_scope_amp_var_for`
(URI::Template's `uri-encode`), #10114 (an exported nested sub reads the
caller's same-named lexical), #10389 (the caller's readonly marks leak into a
closure). Each is a pre-emption of the chain, not a removal of it.

### 1.4 Why the slices so far could not close it

Six ADRs have each removed a cost from one layer of this model, and each was
right on its own terms: ADR-0086 moved the built-in dynamics to a base tier,
ADR-0092 replaced the per-call capture merge with a fallback tier, ADR-0094
measured the kept set and moved the cost to the call side, ADR-9170 made the
system names shared layers, #12511 folded deep captures, and the resolution
paths have per-name memos (`has_multi_candidates_cached_sym`,
`resolve_function_multi_cached_sym`, `fn_keys_for_base`). The workload is still
4.9x rakudo, and the profile is flat because the cost is spread over every
layer of the same design.

The `perf-tuning` skill's §5a arithmetic for #12520's goal (2.45x):

- #12519 alone (capture by lexical scope, keeping the env chain) removes at most
  the capture row: **≤ 1.22x**.
- Adding a per-callsite routine-resolution cache: **~1.45x**.
- The env insert, lookup and flatten rows only go when a call stops building a
  name-keyed env over its caller.

So the goal is reachable only by changing where names resolve, not by
another cache inside the existing layers.

## 2. Decision

**A frame has two links. Every name resolves through the static (outer) link,
except dynamic variables and explicit caller-frame access, which use the
dynamic (caller) link. The frame `Env` stops being a child of the caller's
`Env`.**

### 2.1 Where each kind of name resolves

| name | resolves through | at |
| --- | --- | --- |
| `my` lexical, parameter (`$x`, `@x`, `%x`, `&x`) | its slot, or an upvalue for a free variable (ADR-0018, unchanged) | compile time |
| lexically visible routine (`sub foo`, which is `my &foo`; an imported `&foo`) | a binding in the declaring scope, read like any lexical | compile time; the call site keeps a dispatch cache (§2.4) |
| type, package, `our` name, constant | the symbol table (ADR-0084), never a frame | compile time when the name is known, else one lookup |
| dynamic variable `$*x` | the caller chain: frames that declare dynamics | run time, O(dynamic depth), as in rakudo |
| `CALLER::`, `DYNAMIC::`, `callframe` | the caller chain | run time |
| `OUTER::`, `MY::`, `LEXICAL::`, `EVAL`, `::('$x')`, `&?ROUTINE`/`&?BLOCK` | a name view of the **static** chain, materialized on demand | run time, only when used |

The table removes two things from frames entirely: the caller's names (no
row reads the caller chain for a lexical) and the program's names (types,
packages and constants live in the symbol table). What is left in a frame is
its own slots plus the outer link.

### 2.2 Closure creation captures the outer link, not a copy

Taking a closure records the creating frame's **lexical scope handle** (the
outer link) plus the ADR-0018 upvalues and cells it already records. It does
not walk the caller chain, copy narrow tiers or build layers. Its cost is
O(upvalues) with no dependence on dynamic depth or on how many names the
program declares. `Env::layered_capture`, `CaptureView`, the `Tier::capture_sys`
memo and the capture fallback tier are retired with the caller chain they
summarize (§3, phase 4).

A name a closure body uses that is neither a slot, an upvalue nor a symbol-table
name — the topic-like per-routine names `$_`, `$/`, `$!`, and `self` — is a slot
of the routine frame that owns it, as in rakudo, reached through the outer link
when a nested block reads it.

### 2.3 A call builds no name-keyed env

Call setup binds parameters into slots and sets the outer link (from the code
object) and the caller link (the call-frame stack, already present). It does not
allocate an `Env` overlay, and return does not merge one back into the caller.
Writes a callee makes to a caller's container go through the shared container
or cell (ADR-0018, ADR-0013), which is what makes the `@x` parameter's
copy-out writeback (#12526) unnecessary rather than cheaper.

A frame materializes a name view only when its code asks for one (it contains
`EVAL`, a symbolic lookup or a pseudo-package access, which the compiler already
flags for ADR-0018's per-consumer slot sets). The view is built from the static
chain, so `EVAL` sees exactly the lexical scope of its call site — the §1.3
divergence closes as a consequence, not as a special case.

### 2.4 Routine calls bind once and dispatch through a call-site cache

A call to a lexically visible routine reads its binding like a lexical. A
`multi` binding is a dispatcher value whose candidate list is built when the
scope's declarations are complete, not re-derived from registry keys per call.
The call site caches the chosen candidate keyed by the argument shape
(the types that decide the dispatch) and re-dispatches on a miss. Run-time
`sub` declarations that add a candidate (`EVAL`, `proto.add_dispatchee`)
invalidate the dispatcher's own generation counter, which the call-site cache
checks; nothing else can change the candidate set of a binding.

### 2.5 Cost contract

| operation | after |
| --- | --- |
| lexical read/write | O(1) slot or upvalue (unchanged) |
| closure creation | O(upvalues) |
| call setup | O(parameters), no `Env` allocation |
| routine call, cache hit | O(1) plus binding |
| dynamic variable read | O(caller frames that declare dynamics) |
| `EVAL` / symbolic / pseudo-package | O(names in the static chain), only where used |

Any implementation step that leaves one of these worse records a
`Rakudo:`/`MoarVM:` suffix and an issue, per the `// Cost:` rule.

## 3. Migration

Each phase is its own PR or PRs, passes the gate, and is measured on
FunctionalParsers `t/08` (wall clock, A/B per the `perf-tuning` skill) and on
the bench CI's call-heavy rows (`bench-fib`, `bench-tak`, `method-call`,
`poly-call`, `bench-ctor`, `bench-multi-dispatch`) before it lands. A phase that
regresses one of those rows is fixed on its branch, not landed with a note.

0. **Pins and counters.** Tests for every row of §2.1's table, including
   §1.3's two cases (expected to fail until phase 3, marked `todo`), #12519's
   reproduction, `t/routines/closure/closure-capture-dynamic-depth.t`,
   `CALLER::`/`DYNAMIC::`/`OUTER::` and `callframe` observation (ADR-0035).
   `MUTSU_VM_STATS` counters for env overlays created per call, by-name lookups
   per call and capture entries per closure, so each later phase states what it
   took to zero.
1. **The program's names leave the frame.** Finish ADR-0084: type, package and
   constant names resolve through the symbol table; `__mutsu_*` per-frame
   metadata (`__mutsu_callable_id`, state-scope ids) becomes frame fields.
   Exit: a closure with no free variables captures no system names; ADR-0094's
   and ADR-9170's kept set is empty in the FunctionalParsers profile.
2. **Routine names bind lexically.** `sub` declarations and imports become
   bindings read like lexicals; multis get a dispatcher value and a call-site
   cache (§2.4). Exit: `has_multi_function`, `any_candidate_key_of` and
   `resolve_function_with_types` are absent from the per-call profile of
   FunctionalParsers and `bench-multi-dispatch`.
3. **The static link.** Frames carry the outer link; the reflective name view
   (§2.3) is built from it; dynamic variables and pseudo-packages use the
   call-frame stack; the return merge and the `scoped_child(caller)` overlay
   go. Exit: §1.3's pins pass, #12519 closes.
4. **Retire the dynamic-chain machinery.** Delete `layered_capture`,
   `CaptureView`, the capture fallback, `flatten_scoped_env`,
   `MAX_OVERLAY_DEPTH` and the `filtered_flat_capture` family; mark ADR-0092,
   ADR-0094 and ADR-9170 `Superseded by ADR-12529`. Exit: no frame allocates
   an `Env` unless its code has a reflective consumer.

Phases 1 and 2 are each valuable alone (they shrink the kept set and the
per-call resolution cost) and do not change visible semantics. Phase 3 is the
semantic change; it is the one that needs the pins from phase 0.

## 4. Expected effect

On #12520's profile, phases 1-4 remove the five rows of §1.2's table, ~61% of
the parse, which is ~2.5x against the 2.45x the goal needs. That is an upper
bound, not a promise: what the removed rows leave behind (slot binding, the
outer-link walk for nested blocks) is not free, so the issue may still need a
follow-up once the structure is in place. It is, however, the only plan whose
arithmetic reaches the goal at all.

Beyond #12520, any closure-heavy program pays less: per-call cost stops scaling
with dynamic depth (#12519, #12476) or with declarations in scope (ADR-0094
§3), and the dynamic-scope leaks of §1.3 stop being a class of bug that recurs.

## 5. Risks and open questions

- **Code that depends on the leak.** Anything that works in mutsu only because a
  callee sees its caller's names breaks in phase 3. That code is wrong against
  rakudo, but it may include mutsu's own runtime helpers and bundled modules.
  Phase 0's pins plus `make roast` and the ecosystem ledger are the net; each
  breakage is fixed at its cause, never by restoring a caller lookup.
- **ADR-0092 §7.2** relies on callees resolving a closure's captured names
  through `get_sym`'s fallback pass. Under this ADR a callee never needs them:
  it has its own outer link. Each consumer of that behaviour must be found in
  phase 3, not assumed absent.
- **`$_`, `$/`, `$!` as routine slots.** mutsu currently passes some of them
  through env tiers (the "volatile" names of ADR-9170). Moving them to slots
  touches regex, `given`/`when` and exception plumbing; this is the largest
  unknown in phase 3.
- **GC.** A closure stored in its own outer scope is a cycle through the scope
  handle. The handle must be traced by the cycle collector the way `SubData`'s
  env is today (ADR-9170 §6), or such closures under-collect.
- **Threads.** ADR-0010's spawn-lineage sharing currently materializes names
  into the child's env. With the outer link, a thread clone shares the scope
  handle and its cells; the lineage store shrinks to dynamics and process-wide
  keys. ADR-0018's "a Raku data race may give a wrong answer, never memory
  corruption" must hold for the shared handle (no unsynchronized `&mut`
  through it).
- **JIT.** The JIT's call helpers (`vm_jit_helpers::call_func`/`call_method`)
  go through the same frame setup and must follow it in each phase.
- **No new `Interpreter` fields.** The outer link belongs on the call frame and
  the dispatcher cache on the call site or the dispatcher value, per
  ADR-10779.

## 6. Alternatives rejected

- **#12519 alone (capture by lexical scope, keep the caller-chain env).** Fixes
  the capture leak and removes ≤18% of #12520's parse; the call-side rows
  stay, and so does the `EVAL` leak of §1.3. It is a subset of phase 3, not an
  alternative to it.
- **More per-layer caches** (a resolution memo per call site over the current
  name-keyed path, a flatten memo, a capture memo). This is the shape §1.4
  describes: each is sound, each takes a few percent, and the layers they
  sit in remain. `vm_capture_cache` was such a memo and ADR-9170 retired it.
- **Trim each capture to the names its body references.** ADR-0094 §4 rejected
  it: bare-word type reads are not covered by free-variable analysis, and the
  failure mode is silent. Phase 1 removes the need — type names stop being
  frame names, so there is nothing to trim.
- **Delete `locals` and keep only the env.** Regresses every slot-addressed hot
  path (`docs/vm-dual-store.md`, CP-2), and keeps the dynamic chain.
- **A MoarVM-style frame and moving-GC redesign.** Not needed: the outer link
  is an ordinary `Gc`/`Arc` handle under the existing refcount plus cycle
  collector (ADR-0001), and a moving GC stays rejected.
