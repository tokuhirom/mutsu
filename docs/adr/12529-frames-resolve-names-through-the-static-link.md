# ADR-12529: A frame resolves names through its lexical outer, not its caller — separating the static link from the dynamic link

- **Status**: Accepted (2026-10-10, by tokuhirom; Proposed earlier the same
  day). No phase implemented yet; record each phase's progress in §3.
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
  the caller's dynamic env); the other open issues with the same root cause
  are mapped to phases in §7

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
   **Done** (`test/12529-phase0-pins-and-counters`, base `00ac3b84`):
   `t/vm/scope/lexical-name-resolution-static-link.t` pins one case per §2.1
   row (21 tests, all passing under rakudo). Six fail in mutsu and are
   `todo` with the phase that fixes them: phase 2 for a closure's captured
   `my sub`; phases 1 and 3 for a caller's `my class`; phase 3 for EVAL in a
   closure (it reads the caller's same-named lexical, not its outer), for
   §1.3's two cases, and for a symbolic `::('$x')` lookup. The #12519 shape
   probed there already passes and stays pinned. The counters are a
   `name-resolution` vm-stats line (`src/env/stats.rs`, pinned by
   `tests/name_resolution_stats.rs`): `scoped_overlays`, `chain_walks` /
   `chain_hops`, and `captures` / `capture_own_entries` / `capture_layers`.
   The walk counters are counted in debug builds only, because in a release
   build any call in `Env::get_sym` changed its inlining for +0.4% Ir. With
   them gated out, release Ir is unchanged: +0.003% on a FunctionalParsers
   parse and -0.0002% on `bench-fib`.

   Baseline for the later phases (debug build, one parse of #12520's
   `$ebnfCode13` block including module load): `scoped_overlays=15674`
   `chain_walks=825502` `chain_hops=1801367` `captures=11859`
   `capture_own_entries=120705` `capture_layers=49859`. That is ~10 copied
   entries and ~4 layers per closure, and ~2.2 tiers walked per lookup.
1. **The program's names leave the frame.** Finish ADR-0084: type, package and
   constant names resolve through the symbol table; `__mutsu_*` per-frame
   metadata (`__mutsu_callable_id`, state-scope ids) becomes frame fields.
   This phase is already under way as
   [#7817](https://github.com/tokuhirom/mutsu/issues/7817): its slices 1-7
   (#11092, #11250, #11747, #11820, #11848, #11856, #11882) moved a loaded
   module's callable-id markers, top-level types, enum values, constant and
   type markers, qualified packages and bind markers out of the frame env.
   Per-iteration deep-copy entries on its Cro probe fell from 24,293 to about
   2,600. Its open remainder is this phase's work list: qualified names below
   a module's top level or under an alias, qualified `&` code bindings
   (#11913), and the main program's own top-level routine markers.
   Exit: a closure with no free variables captures no system names; ADR-0094's
   and ADR-9170's kept set is empty in the FunctionalParsers profile.

   **Slice 1 done** (`refactor/12529-phase1-qualified-code-constants`,
   ADR-0084 §7.10): a module's top-level `constant` publishes its qualified
   name (`&FunctionalParsers::alt`, `Pkg::VERSION`) to the package-symbol
   table only. ADR-0084 §7.8 had already routed that store there, but the
   cross-thread publication right after it re-inserted the qualified key into
   the frame env, so the move had not taken effect. `SetGlobalRaw` now says
   whether it publishes a `constant`, and such a store skips the env and the
   shared store. The 12 `&FunctionalParsers::*` names (`constant &alt is
   export = &alternatives` and kin) were in almost every capture layer of the
   parse; they are gone. Same probe as phase 0: `chain_walks` 825,502 →
   666,397 (−19%), `chain_hops` 1,801,367 → 1,380,855 (−23%);
   `scoped_overlays`, `captures` and `capture_layers` are unchanged.

   What the kept set still holds there, per capture (from a dump of the
   captured keys): the per-frame metadata `__mutsu_callable_id`, `&?BLOCK`,
   `__mutsu_block_return_target` / `__mutsu_block_return_owner`,
   `__mutsu_var_source_name::*`, and `self`, `?CLASS`, `__ANON_STATE__`,
   copied into the closure's own tier; and in the shared layers the module
   mainline's `Any`, `?FILE`, `=pod` and its own package name. The topic,
   `$/`, `$!` and `@_` are phase 3's (§5).

   **Slice 2 done** (`refactor/12529-phase1-frame-metadata`): the
   return-targeting ids `__mutsu_callable_id`, `__mutsu_block_return_owner`
   and `__mutsu_block_return_target` are no longer names. They are an
   `Env` field (`FrameIds`) that every derived env inherits: a scoped child
   or block tier from its parent, a flattened env or a closure capture from
   the env it was built from. The capture still knows which routine a
   `return` in the closure targets, but no longer copies three entries into
   its own tier, and the five return-merge loops that skipped
   `__mutsu_callable_id` by name have nothing to skip. Same probe:
   `capture_own_entries` 120,625 → 92,578 (−23%).

   **Slice 3 done** (`refactor/12529-phase1-shadow-meta`):
   `__mutsu_var_source_name::<param>`, the caller variable a `@`/`%`/raw
   parameter was bound from, is *shadow metadata* like `__mutsu_type::`
   (`flags::SHADOW_META`): only `<param>` itself can observe it, so a
   closure capture keeps it exactly when `<param>` is a free variable, and a
   return merge treats it as callee-local when `<param>` is. Same probe:
   `capture_own_entries` 120,625 → 111,150 (−8%; measured before slice 2
   landed), `chain_walks` 663,269 → 526,375 (−21%).

2. **Routine names bind lexically.** `sub` declarations and imports become
   bindings read like lexicals; multis get a dispatcher value and a call-site
   cache (§2.4). The call-site cache is the one
   [#10109](https://github.com/tokuhirom/mutsu/issues/10109) asks for (extend
   ADR-0066's inline cache to multis). There must be one design, keyed and
   invalidated the same way (`fn_resolve_gen`, `.wrap`, a new candidate,
   per-scope operator visibility per #9944), not two caches. Exit:
   `has_multi_function`, `any_candidate_key_of` and
   `resolve_function_with_types` are absent from the per-call profile of
   FunctionalParsers and `bench-multi-dispatch`.

   **Slice 1 done** (`perf/12529-phase2-routine-resolution`): a method body
   enters its own compilation unit, as a sub body (`enter_compilation_unit`)
   and a closure (`call_compiled_closure_with_topic`) already did, so a
   routine name it calls resolves in the compunit it was written in. The
   plain-routine resolution memo (#9081) now keys a compunit-scoped name by
   the executing units as well, which lets it record the unit-private
   fallback and a statically visible package routine for such a name; before,
   a module's own exported routines (FunctionalParsers' `apply`, `many`,
   `alternatives`, ...) paid the whole resolution walk on every call. Same
   probe: `function-full-resolve` 11,581 → 5,383. What is left there is the
   multi families (`sequence`, `success`, `postcircumfix:<[ ]>`), a `constant
   &sp` called by name, and `reduce` -- the multi call-site cache and the
   routine-as-lexical-binding steps below.

   **Slice 2 done** (`perf/12529-phase2-code-var-upvalues`): §2.1's first
   row for `&` names. A closure's call through a captured `&`-parameter
   (`sub apply(&f, &p) { -> @x { &p(@x) } }`) reads the binding by upvalue
   index (`CallOnCodeVar::upvalue`) instead of resolving `&p` through the
   running frame's env. Only a readonly `&`-parameter -- no `is copy`, `is
   rw` or `is raw` (`CompiledCode::readonly_code_params`) -- is frozen into
   the upvalue array by value, since nothing can rebind it during the
   invocation; a `my &f` keeps the by-name read, because mutsu's mutation
   analysis cannot vouch for it (ADR-0018). An enclosing closure that only
   hands such a binding on to a nested one carries it as a transit
   upvalue.

   **Slice 3 done** (`perf/12529-phase2-multi-probe-index`): the `&self`
   multi-candidate probe `resolve_function` runs for every by-name `&name`
   read walked the whole functions map; it now reads a base-name index entry
   the `&mut` paths filled, and an attribute-twigil name (`&!pGExpr`, which
   reaches `resolve_function` when the attribute read misses) answers `None`
   at once, since no routine can be named `!x`. FunctionalParsers probe,
   release build, whole run: 4.03G → 3.87G Ir (−4%).
3. **The static link.** Frames carry the outer link; the reflective name view
   (§2.3) is built from it; dynamic variables and pseudo-packages use the
   call-frame stack; the return merge and the `scoped_child(caller)` overlay
   go. Exit: §1.3's pins pass, #12519 closes.

   **Slice 1 done** (`perf/12529-phase3-closure-capture-frame-root`): the
   first piece of the outer link, for closures created while a closure
   body runs. `Env` marks the root tier of a call frame (`frame_root`, set
   by `scoped_child`), and `layered_capture` stops its walk at the running
   frame's root when that root holds a closure capture: the new closure
   takes the body's own tiers and its capture, plus the chain's flat tail
   (the program scope, which later slices replace), never the caller
   frames in between. A capture made at the bottom of a 40-deep nest of
   closure calls now has as many layers as one made at depth 5
   (`tests/name_resolution_stats.rs`). A frame with no capture of its own
   -- a named sub's -- still keeps its whole chain; its lexical outer is
   the next step.

   **Slice 2 done** (`perf/12529-phase3-named-sub-capture`): the same cut
   for every frame. A closure created in a named sub's frame takes that
   frame's tiers and the program-scope tail, not the frames that called the
   sub: a named sub has no capture of its own, and its free variables
   already resolve through the declaration-scoped stores (unit lexicals,
   ADR-0024) rather than through its callers. Same FunctionalParsers probe:
   `capture_layers` 47,961 → 23,187 and `capture_own_entries` 81,639 →
   67,246.

   **Slice 3 done** (`perf/12529-phase3-detached-routine-frames`): the
   first lookups that resolve through a static link rather than the caller
   chain. A named routine declared outside every routine body
   (`CompiledCode::declared_in_routine` clear) has the program scope as its
   lexical outer. When its own code looks names up reflectively (`EVAL`, a
   symbolic `::('$x')`, a pseudo-package; `links_static_outer_to_unit`), its
   frame root carries a `StaticLink::UnitOuter` (`src/env/static_link.rs`)
   pointing at the program scope's tiers -- the chain below the deepest
   caller frame. A by-name lookup of a plain user lexical that misses the
   frame continues there, past every caller frame; dynamics, `self`, the
   topic and system names still walk the whole chain, and so do the
   flatten family's copies (`apply_static_links`). The pins for §1.3's sub
   case and the symbolic lookup pass. Only frames that ask get the link, so
   the call paths that reuse or skip a frame root (`overlay_is_shared_empty`
   reuse, the fast path's unscoped calls) give such a body a root of its
   own and are unchanged for everything else.

   Two limits remain. `EVAL $code, context => $ctx` resolves through the
   whole chain while it runs, because a `PseudoStash` does not carry a
   frame's lexicals yet; vendored `Test`'s string `throws-like` depends on
   it. And a closure's frame is not linked: its capture is a snapshot below
   the caller chain, so moving it above the chain needs the capture to share
   cells with the creating frame first (the closure EVAL pins).
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

## 7. Open issues with the same root cause

These issues were filed before this ADR. Each one traced a symptom back to
names resolving through the shared, caller-chained env, and each proposes a
local remedy. Under this ADR they become work items of a phase, or are closed
by one. A slice that works one of them follows the phase's design rather than
the issue's original local remedy.

| issue | what it is | phase |
| --- | --- | --- |
| [#12519](https://github.com/tokuhirom/mutsu/issues/12519) | closures capture the caller's dynamic env | 3 (closes) |
| [#12478](https://github.com/tokuhirom/mutsu/issues/12478) | a named `sub` in a recursive `.map` callback reads another activation's lexicals: the hoisted sub resolves them by name through the shared env (blocks SBOM::CycloneDX `t/90-valid.rakutest`) | 3 (closes). The issue's "decline the inline loop" is a workaround |
| [#10511](https://github.com/tokuhirom/mutsu/issues/10511) | `EVAL`'s undeclared-variable check reads the runtime env, not the static pad | 3: the reflective name view of §2.3 is the static pad it asks for |
| [#8335](https://github.com/tokuhirom/mutsu/issues/8335) | a closure call has no light path. Of its remaining 9,176 Ir/call, ~4,300 is the env frame (`Env` create/drop, caller-env push/pop, four `insert_sym`) | 3 removes the frame; 1 removes the `&?BLOCK` / callable-id inserts. Its "option 2" keeps `set_capture_fallback`, which phase 4 deletes, so take it only as a stopgap |
| [#9170](https://github.com/tokuhirom/mutsu/issues/9170) | scope entry/exit and closure creation walk the env. The open remainder: `ImportScope` key-set snapshot, `RoutineScope`'s O(routines) restore diff, the BlockScope-exit sigilless-alias sync | 2 (routine and import scope stop being registry snapshots); 4 retires what ADR-9170 built for its closure rows |
| [#10109](https://github.com/tokuhirom/mutsu/issues/10109) | a cached multi call costs 2.4x a plain sub | 2: one call-site cache design (see phase 2) |
| [#10419](https://github.com/tokuhirom/mutsu/issues/10419) | a block-scoped `use` import leaks into a later `EVAL` because the alias and the module's own definition share a global registry key | 2: an imported routine is a lexical binding, so it has no registry key to collide on. Reconcile with the ADR-0081 amendment its re-triage asks for |
| [#9171](https://github.com/tokuhirom/mutsu/issues/9171) | pseudo-stash reads (`GLOBAL::`, `DYNAMIC::`, `PROCESS::`, a whole `P::`) and `PackageScope` still build or diff the whole env | 1 for package stashes (a real symbol table), 3 for `DYNAMIC::` (the call-frame stack) |
| [#7817](https://github.com/tokuhirom/mutsu/issues/7817) | ADR-0084's tracker | 1 (see phase 1) |
| [#12526](https://github.com/tokuhirom/mutsu/issues/12526) | an `@x` parameter costs ~12x a `$x` one (generic binder + copy-out writeback) | partly 3: the writeback becomes unnecessary (§2.3). The binder half is independent and can land first |

**Out of scope.** [#12204](https://github.com/tokuhirom/mutsu/issues/12204)
(the cross-thread shared store is keyed by bare name; `tier:icebox` by a
maintainer decision) has the same character: names instead of bindings as
identity. Phase 3 makes keying that store by binding identity natural, but
this ADR does not reverse the icebox decision, and does not commit to
re-keying the store.

## 8. Size estimate

**About 30 slices (range 23-37), where a slice is one merged PR that passes
the gate.** Phase 3 carries most of the uncertainty. The estimate was made
on 2026-10-10 against `main` @ `595ec6eb`, from the size of what each phase
touches and from how comparable campaigns went. Re-estimate after phase 0
and again after the first phase-3 slice, and record the new figure here.

| phase | slices | what sets the number |
| --- | ---: | --- |
| 0. pins and counters | 1-2 | One test file per §2.1 row, plus the `MUTSU_VM_STATS` counters |
| 1. program names leave the frame | 4-6 | #7817 took 7 slices to cover a loaded module's top level. The rest is that issue's three open items, plus the same move for the **main program's own** declarations (types, routine markers, `?CLASS`/`=pod`/`?FILE`/`Any`), which #7817 never covered. `__mutsu_callable_id` and the state-scope ids are read in 63 files |
| 2. routine names bind lexically | 6-10 | 107 `has_multi_function`/`has_multi_candidates` call sites, 45 `resolve_function*`, 46 direct `functions` registry lookups and 98 `fn_resolve_gen` references. Operators are routines too, and they also have per-scope visibility (#9944). Order: the dispatcher value, then plain subs, then multis plus the call-site cache (#10109), then imports and `my sub` scopes (#9170's `ImportScope`/`RoutineScope` restore, #10419), then `EVAL`-added candidates and `.wrap` |
| 3. the static link | 10-16 | `Env::scoped_child` is installed at 30 sites in 12 files, over about six call paths (named sub, closure, fast/light, typed light, method, `call_sub_value`). The return merge and writeback have 32 sites. The topic, match and error names have ~700 references in `src/vm` and `src/runtime`. The reflective consumers (`CALLER::`/`OUTER::`/`DYNAMIC::`/`LEXICAL::`, `EVAL`) span 74 files. Thread cloning has 99 references |
| 4. retire the machinery | 2-3 | `CaptureView`, `layered_capture`, the capture fallback and the flatten family span 24 files, with 53 references to the flatten/overlay-depth helpers alone; `src/env_capture_view.rs` (399 lines) and most of `src/env_tier.rs` (654) go. Deletion only, once phase 3 leaves them unreachable |

### Why phase 3 has the widest range

Phase 3 flips where names resolve, and ADR-0039's slice 2, the closest
precedent, needed **four attempts**. Three were withdrawn after the gate or
the battery run named a consumer that depended on the old resolution
(ADR-0039 §§9-13). Expect the same here. The range assumes:

- **One call path per slice.** Closures first: that is #12519, the
  narrowest, and it unblocks #12478. Then named subs, methods, and the
  fast/light paths. The reflective name view (§2.3) lands with the first
  path that needs it.
- **The topic/match/error move as its own 3-4 slices** inside the phase,
  one per family (`$_`, `$/` with regex captures, `$!` with
  `CATCH`/`try`). This is the largest single unknown (§5).
- **Two to three withdrawn or re-landed attempts.** That margin is the
  difference between 10 and 16.

### What the estimate does not cover

- **#12520's ratio after the structure is in place.** §4 bounds the gain at
  ~2.5x against a 2.45x target. Closing that issue may still need a slice
  or two of ordinary tuning once phases 1-4 have landed.
- **Breakages in the ecosystem ledger** that only phase 3's flip reveals.
  They are fixed at their cause (§5) and counted against whichever phase
  surfaced them, so a large batch moves the phase-3 figure, not this table.
- **#12526's binder half.** It is independent of this ADR and can land
  first.
