# ADR-12529: A frame resolves names through its lexical outer, not its caller — separating the static link from the dynamic link

- **Status**: Accepted (2026-10-10, by tokuhirom; Proposed earlier the same
  day). Phase 0 is done and phases 1-3 are in progress; record each phase's
  progress in §3.
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

**First step landed** (`perf/10109-multi-call-site-cache`, #10109): the
call-site cache half, ahead of the binding half. A bare multi call of a
type-deterministic family (`func_multi_dispatch_type_cacheable`) keys its
whole dispatch by (package, name, argument type keys) in `multi_light_cache`,
the key `func_multi_resolve_cache` already uses for the winner. The generic
path fills it when such a call took the positional light bind; a later call
with the same types goes from `CallFunc` straight to the winner's light bind
with the dispatch frame and samewith context (`vm_multi_light_call.rs`),
re-checking only what can differ per call: the light bind's admissions, a
wrapper, the mainline-capture block. The registry generation stands in for
the dispatcher's own counter until the binding is a value: an inner scope's
candidate, an `EVAL`ed one or a `wrap` retires the entries as they retire
`func_multi_resolve_cache`'s. The repro of #10109 went from 13.3k to 8.7k Ir
per call, against 9.8k for the plain sub. Value-dependent families (`where`,
subsets) still take the generic path; #9967's `BareMultiPlan` is their
equivalent. Since phase 2 slice 7 the family's own generation backs the
registry generation: an entry survives another family's registration (§3).

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

   **Slice 4 done** (`fix/12850-block-sub-lexical-binding`, #12850, #12851):
   the first routine-as-binding step. A plain `sub` declared in a block,
   loop body or closure body is that block's `&name` binding for the
   closures nested in it, so a closure keeps the routine of the activation
   it was created in: after the block exited (a bare call used to die with
   "Unknown function", because an inlined block restores the registry on
   exit) and while a later activation runs (it used to call the latest
   activation's routine, through the latest activation's cells).

   - The compiler records the plain routines each block scope declares
     (`compiler/block_lexsubs.rs`): a single, non-`our`, non-exported `sub`
     with an identifier name no builtin answers to and no user trait. An
     inline block, a `for` body and a closure body record them at the hoist;
     a block that does not hoist (a `while` body) at the declaration. A
     routine body's own top level does not: ADR-0113 keeps a call-only inner
     sub frame-lexical there, and a routine activation already binds its
     nested subs' free variables (ADR-0114).
   - A closure body inherits the records of the scopes around it (a method
     or anonymous `sub` body does not), and compiles a bare call `g(...)` to
     one of them as `&g(...)` (`CallOnCodeVar`), unless it declares a routine
     or a `&g` of that name itself. It lists the name in
     `CompiledCode::lexsub_refs`, and so does every enclosing closure that
     only hands it on. `compute_upvalues` makes `&g` an upvalue, for the
     calls and, through `code_var_upvalue_reads`, for a `&g` value read.
   - The frame that creates such a closure fills the upvalue
     (`bind_lexsub_upvalues`): code that declares `g` with the code object of
     its own activation (slice 9 of phase 3's `capture_lexsub_activation`),
     code that hands it on from its own upvalue array. The closure also
     carries the binding in its captured env, which is what a runtime
     recompile of its body reads by name (the inline `map` path seeds the
     names as enclosing routines, `seed_amp_shadowed_calls_from`).

   Left on the registry: a block-declared routine's own body calling a
   sibling routine of its block by name after the block exited (it is a
   named routine body, compiled without the block's records; #13037, slice
   5), `multi`/`our`/exported/traited routines, and a routine named like a
   builtin (`sub dd`).

   Release-build Ir against the merge base: `bench-fib` -0.00%, `bench-tak`
   +0.00%, `method-call` +0.01%, `poly-call` -0.01%, `bench-ctor` +0.14%,
   `bench-multi-dispatch` -0.01%, `bench-string` +0.15%. The shape the slice
   changes costs more: a closure calling a loop body's routine +6.8%, and
   +30% through the inline `map` path, which has no upvalue array and
   resolves `&h` by name per call. The call goes through a code object's
   closure dispatch instead of the named call path; #13049 tracks it.

   **Slice 5 done** (`fix/13037-block-sub-sibling-binding`, #13037): a
   block-declared routine's own body reaches the sibling routines of its
   block. Its body stays a named routine body (its calls keep the named call
   paths); the binding lives in the activation instead.

   - `capture_lexsub_activation` also binds, as `&name` in the activation
     capture, each sibling routine of the declaring code whose name the
     constant pool of the routine or of a closure nested in it holds (a
     by-name call reads its callee from there). Each binding is the sibling's own capture of the same
     activation, so a chain `a → b → c` keeps the activation all the way. A
     cycle (`ev`/`od` calling each other) binds its back edge to a cell that
     receives the routine's capture once it is built.
   - A `CallFunc` in a frame linked to an activation that binds `&name`
     (`Env::frame_activation_binds_routine`, O(1) in every other frame)
     calls that binding through the code-variable call path, ahead of the
     registry, which holds only the latest activation's routine (none once
     the block exited).

   Left on the registry: the declarations slice 4 leaves there.

   **Slice 6 done** (`fix/12529-block-multi-binding`): the first step of
   §2.4's binding half. A `multi` family declared in a block is that block's
   `&name` binding for the closures nested in it, as a plain `sub` is since
   slice 4, so a closure calling it after the block exited stops dying with
   "Unknown function".

   - `plain_lexsub_name` (`compiler/block_lexsubs.rs`) admits `multi`
     declarations under the same exclusions (`our`, exported, traited,
     builtin-named). The binding the creating frame fills is the family's
     dispatcher value (`sub_value_from_multi_candidates`): every candidate
     visible at the block, an outer scope's included, captured when the
     closure is created. That is the "candidate list built when the scope's
     declarations are complete" of §2.4, built per closure creation for now.
   - A dispatcher whose candidates are still live dispatches by name, as
     before. Once they are out of scope it ranks its captured candidates
     (#12474) and now runs the winner as a dispatch
     (`call_captured_multi_winner`): inside a multi-dispatch frame over the
     other captured candidates that accept the arguments, so `callsame` /
     `nextsame` defer to them, and with a samewith context that re-dispatches
     through the dispatcher value. Closure dispatch does not replace that
     context with the candidate itself (`samewith_context_is_family_of`).
     Both were broken for any out-of-scope dispatcher value (`my &n; { multi
     ns ...; &n = &ns }; n(7)` died with "callsame is not in the dynamic
     scope of a dispatcher").
   - Candidates are not re-closured per activation: a candidate reads its
     free variables through the env captured with the dispatcher. Rakudo
     does not clone multi candidates per block entry either, so a closure
     made in each iteration of a loop that declares a `multi` answers with
     the *last* iteration's lexicals there (`for 1, 2 -> $v { multi m(Int) {
     $v }; @a.push: -> { m(0) } }` gives `2 2` in rakudo); mutsu gives `1 2`.
     Neither is pinned.
   - The dispatcher records the registry generation its candidates were
     gathered under. While that generation is current (the declaring block
     is live) and the package matches, the closure's `&q(...)` takes the
     `CallFunc` path and its multi light cache
     (`family_dispatcher_is_current`); without it the call went through
     `call_sub_value`'s by-name resolution and cost 80.7k Ir instead of 28.8k
     (`perf/12529-block-multi-callfunc-path`). Out of scope,
     `captured_candidates_are_live` compares the compiled candidates'
     fingerprints, not only package and name: a later block's own `multi
     q7(Int)` used to take a call meant for the captured one.

   Left on the registry: a multi family's generation is still the registry's
   `fn_resolve_gen`, a multi *method* call has no cache in front of the
   resolver, and value-dependent families take the generic path.

   **Slice 7 done** (`perf/12529-multi-family-generation`): a multi family
   has its own generation. The family memos (`multi_candidates_cache`,
   `func_multi_type_cacheable`, `func_multi_argkey_cacheable`,
   `func_multi_resolve_cache`, `multi_light_cache`, `bare_multi_plan_cache`)
   are `FamilyGenCache`s (`runtime/family_gen_cache.rs`).

   - Each base name of the functions map has a generation in
     `DispatchState::fn_family_gens`, bumped by `evict_fn_keys_base`. That
     eviction is the contract every functions-map write already keeps for the
     key index, and the debug audit of the index catches a write that misses
     it. A change that is not a key change (`wrap`, an import scope popping)
     still goes through `invalidate_fn_resolution`, which clears the memos.
   - An entry keeps `fn_resolve_gen` as its fast path, so a steady-state call
     pays no family lookup. Only when the map generation moved does the probe
     read the family's generation; an entry whose family is unchanged is
     re-tagged with the running map generation and served.
   - The case it fixes: a block that declares a routine of its own moves the
     map generation on every entry and exit, which retired the multi caches
     of every other family. `for ^3000 { { multi g(Int) {...}; f($i) + g(1) } }`
     ran `f`'s full candidate walk 3000 times; it now runs it once
     (`tests/multi_family_generation.rs`).
   - A hit reads through a shared lookup and clones the answer; the re-tag
     takes the mutable lookup on a cold path. A first cut that took the
     mutable lookup on every hit, and read the family generation on every
     miss, cost `bench-multi-dispatch` +0.5% Ir.

   Release-build Ir against the merge base: that repro -40.4%,
   `bench-multi-dispatch` +0.02% (+0.10% with the JIT), a plain multi call
   loop +0.10%.

   Left on the registry: the plain-routine memos (`fn_resolve_cache`,
   `plain_fn_resolve_memo`, the light-call caches) are still tagged with the
   map generation only; a multi *method* call has no cache in front of the
   resolver; value-dependent families take the generic path.

   Measured after slice 7 (release callgrind, Ir per call): the plain-routine
   memos need no family generation, because their map version alternates
   between two states `GenCache` keeps both of (`for ^3000 { { sub g {..};
   f($i) } }` resolves `f` twice in all); a multi method call costs 24.1k
   against 23.2k for a plain one, so a cache in front of its resolver buys
   little; a `where` family cost 18.0k against 10.8k for a type-only one,
   almost all of it the generic path around a cheap inline `where`.

   **Slice 8 done** (`perf/12529-where-multi-light-call`): a value-dependent
   family takes the light call path (`vm/vm_multi_plan_light_call.rs`).

   - `multi_plan_light_cache`, keyed like `multi_light_cache`, holds the
     family's keyed `BareMultiPlan` and the light body of every candidate its
     stage program can pick, found by body fingerprint among the family's
     keys when the generic path first light-binds a winner. A call with the
     same argument types runs only the stage program and goes to the
     winner's positional light bind, with the dispatch frame and samewith
     context of the type-deterministic hit.
   - The value checks are user code and must run once per call: every
     candidate's light-bind admission is decided before the program runs, so
     a winner it picks is always taken. Only "no winner" falls back, and the
     generic path then raises the dispatch failure. Only a program whose
     checks are all inline WhateverCode `where`s is taken (a `subset`
     predicate may be any block); a refused family is remembered per
     `(package, name)` (`multi_plan_light_refused`).
   - `multi f(Int $x where * > 0)` + `multi f(Int $x)` called 3000 times:
     -26% Ir, one full resolution instead of 3000. `bench-multi-dispatch`
     +0.22% (+0.37% with the JIT): its `where` families use `subset`s and are
     refused, and each of their generic light calls pays the refusal probe.
     Other rows within ±0.06%.
   - Found on the way: a block-declared multi runs its winner's `where`
     twice per call on every path (#13183).
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
   topic and system names still walk the whole chain. Only lookups follow
   the link: a flattened copy stays faithful to the whole chain, since a
   deep chain or a frame's saved env *is* its flattening, and the writeback
   that carries a runtime-named write across frame exits reads where the
   value is (`Env::get_through_callers`), not what the name means there.
   The link lives on the root's overlay tier (`Tier::static_link`), not on
   `Env`, which every call moves and clones; a chain-level latch
   (`chain_has_static_link`, like `chain_has_fallback`) sends only chains
   that hold one through the walk that follows it. Release-build Ir against
   the merge base, same machine: `bench-fib` +0.20%, `bench-tak` -0.50%,
   `method-call` -0.03%, `poly-call` -0.07%, `bench-ctor` +0.16%,
   `bench-multi-dispatch` +0.29% -- within the layout noise of two builds.

   **Slice 4 done** (`perf/12529-phase3-reflective-closure-link`): a
   closure that looks names up reflectively links its frame too, which
   closes the two closure pins. Its capture is built as the creating
   frame's static view (`Env::flattened_static_view`: a static link in the
   creating chain hides what it skips), so a closure made in a callee no
   longer carries that callee's callers. Created while the program scope
   itself runs (no frame root in the chain), the capture is marked
   (`Tier::program_scope_capture`) and the closure's frame links to the live
   program scope. Created inside a call frame, the link is `capture_first`:
   the capture answers a plain lexical before the program scope, past the
   callers. For that to stay live, the compiler puts every plain scalar of a
   frame that creates an escaping reflective closure into
   `needs_cell_locals`, and the capture boxes the visible ones into shared
   cells first (`box_locals_for_reflective_capture`), as an escaping
   closure's mutated free variables already are (ADR-0018).

   Which code is linked is now `CompiledCode::static_name_lookup`: code
   (or a closure nested in it) that names a lexical by string -- `EVAL`, a
   symbolic or `::($name)` lookup, a pseudo-package. Slice 3 keyed on
   `needs_reflective_capture`, which also covers bodies that only *run*
   against the live env by name (`gather`, `whenever`, a package body) or
   observe callers (`callframe`); linking a `react` body made a `whenever`
   callback resolve `$t` in the program scope instead of the react frame.
   `CALLER::`/`CALLERS::` read the caller frame through the whole chain
   (`Env::get_through_callers`), whatever links it holds.

   **Slice 5 done** (`fix/12529-my-class-symbolic-scope`): a static link
   also skips the caller frames for a bare type-shaped name (an unqualified
   identifier starting with an uppercase letter), which closes the `my
   class` pin. A caller frame binds such a name only for a type it declared
   lexically; a package-scoped type is reached through the symbol table,
   so `::('T')` in a callee still finds an `our class T` the caller
   declared, and no longer finds its `my class T`.

   **Slice 6 done** (`perf/12529-phase3-unit-routines-link`): a routine's
   static link is a frame flag (`Env::outer_is_unit`, packed into the env's
   padding) instead of a link stored on the frame's overlay tier, so marking
   a frame allocates nothing; the program scope's position is found when a
   lookup first crosses the frame. Which routines count as declared at
   program scope is now `CompiledCode::outer_is_unit`, which also excludes a
   sub declared inside a closure body (`Compiler::in_closure_body`): slice 3
   linked `call { my $x; sub g { EVAL q[$x] }; g() }` past the block that
   declares `$x`. A frame is marked only once its arguments are bound,
   since binding resolves each argument's source variable in the caller's
   scope. A CONTROL
   handler, whose body runs on the raise site's env, runs with links
   suppressed as well.

   Linking *every* program-scope routine frame was tried on the same
   branch: 6 of 7128 `t/` files failed, each a piece of call machinery
   reading a caller-scope name through the callee's env. Five are fixed by
   the changes above; the last, the light-call `is rw` writeback, needs that
   convention to share a cell end to end instead of writing back by name.
   It is tracked as #12641, the next prerequisite of this phase.
   Release-build Ir against the merge base: `bench-fib` +0.16%,
   `bench-tak` +0.31%, `method-call` +0.08%, `poly-call` +0.59%,
   `bench-ctor` +0.08%, `bench-multi-dispatch` +0.02%. A first cut cost
   ~1% on the call rows: a per-call `Option` guard on the light and fast
   paths, and `ArrayData::items` falling out of line as the call path grew
   (now `#[inline]`).

   **Slice 7 done** (`fix/12641-rw-writeback-through-cells`, #12641):
   every routine declared at program scope now links its frame
   (`CompiledCode::links_frame_to_unit` is `outer_is_unit`), not only
   those that name lexicals by string. #12641's last blocker, the light-call
   `is rw` writeback, turned out not to need a new calling convention: it
   broke because slice 6 stopped `Env::scoped_child` from chaining a new
   frame past an empty marked caller root, and the writeback depends on that
   chain shape. Marking a root no longer pins it in the chain. The named
   call path marks its root at creation and runs the binder with links
   suppressed, since a binder-opened tier can make the post-bind env a
   non-root. Two rules keep the dynamic machinery working:

   - Only the frame a lookup starts in decides where its lexicals continue:
     the walk applies the link of the first frame root it meets and ignores
     the links of caller frames beyond it. A closure called from a linked
     routine (`lives-ok { ... }`) walks on past that routine into its own
     block, where `roast/S06-signature/type-capture.t`'s `my class Foo` lives
     (the global `class Foo` won otherwise).
   - The fast path forces a root only for a body that names lexicals by
     string; any other program-scope routine that would run in its caller's
     env still does, unmarked.

   With every program-scope routine linked, the whole `t/` tree and the
   whitelisted roast pass. Release-build Ir against the merge base: `bench-fib`
   +0.36%, `bench-tak` +0.18%, `method-call` -0.43%, `poly-call` +0.00%,
   `bench-ctor` -0.33%, `bench-multi-dispatch` +0.12%. A first measurement
   showed +0.4% to +1.1%, which was none of this slice's logic:
   `nanbox::payload_op`, the dispatch every heap clone and drop goes through,
   had doubled in size because the optimizer inlined two `Gc::drop`s into
   it. Its release path is now out of line (`release_gc`), which keeps it
   the same size whatever the codegen-unit split.

   **Slice 8 done** (`fix/12478-flattened-frame-program-scope`, #12478):
   methods of a class declared at program scope link the frames they get, and
   the link survives a flattened caller. Before this slice a linked frame
   found the program scope as "the env below the deepest frame root". A
   method dispatch or a closure capture flattens the running frame
   (`flatten_scoped_env`, `MAX_OVERLAY_DEPTH`), and the flattened env merges
   that frame's names with every caller's. A frame called from there skipped
   its callers straight into theirs, so a recursive call's `my $x := $t`
   bound the outer activation's `$t` cell. Four changes:

   - A flatten that replaces a live frame's chain records the program scope
     its frames ran over on the collapsed tier, as a `StaticLink` of a
     non-root env (`Env::flattened_recording_program_scope`); a closure
     capture's flatten does not. The record is inert for lookups through
     the flattened env itself. `program_scope_from`,
     `link_static_outer_to_unit` and the next flattening read it.
   - The light, typed-light and fast call paths mark a linked frame before
     seeding its locals from the env, so a local never starts as a caller's
     same-named variable. The light path lifts the mark again for the binder,
     which resolves the arguments in the caller.
   - A `:=` bind whose source has a slot in the running code takes its cell
     from that slot or this frame's own overlay, never from a caller's
     same-named binding.
   - Attribute variables (`$!t`) are lexically scoped for the link. A bind of a
     scalar attribute aliases the attribute's own cell on its invocant (the
     invocant is the attribute's home, as the compunit store is a unit
     lexical's), and is no longer published under `!t` in the env or the
     ancestor frames. A callee running on another invocant read that entry as
     its own attribute. #12478's repro overwrote the inner node's `$!t`.

   A role method keeps resolving through its frame chain, because the role's
   parameters are a scope between the method and the program scope. A method
   that creates closures still runs in its caller's env without a root of its
   own, as before, so it is not linked: giving it a root costs a flatten on
   every method call in its body (+1.6% on `bench-ctor`), and the closure
   frames above it carry its names through their captures.

   The real SBOM::CycloneDX `t/90-valid.rakutest` exposed three more callers
   of the leak, fixed in this slice:

   - The closure-return writeback asked the caller's env whether it holds a
     name, through the caller's static link, so a rebind made in a closure
     called by a program-scope routine was dropped
     (`roast/S02-types/array-shapes.t`). It now searches the caller chain, as
     the other writeback paths do.
   - A regex literal in a routine did not capture a kebab-case lexical it
     interpolates (`$full-path`); the match found it in the caller chain, which
     a linked matcher (`Test::like`) no longer reaches
     (`roast/S24-testing/line-numbers.t`). It is captured from its slot, as a
     short name is.
   - `my %m is Map = a => Nil` stored `Any`, and a sub's per-activation alias
     of a `Nil`-bound lexical kept the previous loop iteration's cell.

   Release-build Ir against the merge base: `bench-fib` -0.12%, `bench-tak`
   -0.06%, `method-call` -0.00%, `poly-call` +0.01%, `bench-ctor` +0.19%,
   `bench-multi-dispatch` +0.03%.

   What is still dynamic: a reflective *write* (`EVAL q[$x = 1]`) lands in
   the frame's overlay and the return merge carries it to the caller, as
   before; only reads follow the link. The pins for §1.3's sub
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

   **Slice 9 done** (`fix/12692-block-nested-sub-activation`, #12692): a
   named sub's code object closes over the activation that declared it, as
   rakudo's `capturelex` re-closures a sub on every entry of its block. A
   sub declared in a loop body or a `.map` callback read its free variables
   through the stores keyed by its *name* -- ADR-0024's unit-lexical
   buckets, mutsu#9111's aliases (which only a routine body binds per
   activation), TRIR's outer bindings -- so every `&g` taken in an earlier
   iteration answered with the latest iteration's lexicals.

   - A `&g` read in the code that declares `g` boxes `g`'s free variables
     that are locals of that code into shared cells, in place, binds them in
     the code object's capture and marks it (`Env::mark_activation_capture`,
     `vm/vm_lexsub_capture.rs`).
   - Calling such a code object links the frame capture-first through the
     static link, and the link itself carries the capture
     (`StaticLink::UnitOuter::activation`), so a flattening of the frame's
     chain (`.map` and other method calls flatten the running frame) keeps it
     in its record.
   - The per-name stores ask `Env::frame_capture_holds` and step aside for
     every name the activation binds: `unit_lexical_slot` and its `_mut` and
     container-cell twins, the closure-capture injections, and TRIR's
     `trir_declared_binding`; TRIR caches no outer binding in such a frame.
   - A closure created in such a frame closes over the same activation (its
     capture is marked too), so `sub g { (1, 2).map({ $q + $_ }) }` reads its
     own iteration's `$q`.

   Nothing looks through the caller chain to find the activation. A by-name
   call `g()` inside the block still resolves through the routine registry
   and the latest activation's cells, which is the current activation as
   long as the block is not re-entered recursively; that is phase 2's
   routine-as-lexical-binding step, which also covers what is left of the
   shape: a `&g` taken from a closure nested in the declaring block is not
   an activation capture (#12851), and a closure that calls `g()` by name
   after the block exited finds no registry entry (#12850).
   Release-build Ir against the merge base: `bench-fib` -0.00%,
   `bench-tak` -0.00%, `method-call` -0.09%, `poly-call` -0.03%,
   `bench-ctor` +0.03%, `bench-multi-dispatch` -0.32%.

   **Slice 10 done** (`fix/12398-routine-match-var-scope`, #12398, #12720):
   the first of the topic/match/error families (§5, §8), `$/`. A routine
   owns its `$/` and the capture views that belong to it (`$0`, `$<name>`);
   a block shares the `$/` of the scope it is written in. mutsu keeps the
   match state as env names, which a routine's writes already kept out of
   its caller (the return merge skips them), but a read that missed the
   routine's frame walked on into the callers: a sub started with its
   caller's match, and `m:c` continued from the caller's position. Nothing
   is inserted per call:

   - A routine's frame root is marked as the owner of the match state
     (`Env::mark_owns_routine_state`, set by the named, light, typed-light,
     fast, method and closure-dispatch paths for a routine). A lookup of a
     match-state name (`Symbol::is_routine_owned`) stops there
     (`env/routine_owned.rs`); the walk is selected by a chain latch, like
     `chain_has_static_link`, so every other lookup is unchanged. A block's
     frame root answers `$/` from its capture instead of its caller; its
     capture views go on to the caller, where `.subst` / `.trans` publish
     them per match, and a miss derives them from `$/`.
   - A flattening of a chain with an owner (a method call flattens the
     running frame) is settled to answer each match-state name as the live
     chain does (`Env::settle_routine_owned`), so it does not import the
     callers' match.
   - Which code touches the match state is a compile-time fact
     (`CompiledCode::uses_routine_state` / `sets_routine_state`). The fast path
     gives such a routine a root, the light paths do not reuse a frame that
     does not own one, and a block that touches it shares its scope's `$/`
     through a cell boxed at creation. That used to cover only blocks with a
     regex `~~`; `.match`, `.subst` and plain reads did not reach the scope
     (#12720).
   - One setter for a match result (`Env::assign_match_var`): when the
     visible `$/` is a shared cell it stores through it, so a block made
     before its scope matched again still reaches the scope's `$/`; `.subst`
     and `.trans` publish and restore the per-match `$/` the same way
     (`publish_match_var` / `restore_match_var`).

   Release-build Ir against the merge base (interpreter lane / JIT lane):
   `bench-fib` +0.18% / +0.33%, `bench-tak` +0.12% / +0.18%, `method-call`
   +0.14% / +0.16%, `poly-call` +0.11% / +0.08%, `bench-ctor` +0.11% /
   +0.11%, `bench-multi-dispatch` +0.35% / +0.34%, `bench-string` +0.92% /
   +0.54% (about 30 Ir per match for the setter's walk). A first cut cost
   +1% on `bench-multi-dispatch`: the flattening fixup replaced a caller's
   `Nil` `$/` and so copied a map the flattening shared, and the lookup's
   miss path read the symbol's flag word twice. Unset and `Nil` now count as
   the same, and one read answers both questions (`symbol::env_key_probe`).

   Still open: a method that creates closures runs in its caller's env
   without a root (slice 8), so it still reads its caller's `$/` until it
   matches (#12882); the capture views stay mirrored env keys rather than views
   derived from `$/`; and `$_` and `$!` are the remaining families.
   **Slice 11 done** (`fix/12902-routine-owned-error-var`, #12902): the
   `$!` family, on slice 10's mechanism. The walk now covers every
   routine-owned implicit (`Symbol::is_routine_owned`, the existing
   `ROUTINE_SCOPED_IMPLICIT` flag: `$!`, `$/` and its capture views;
   `env/routine_owned.rs`, renamed from `match_state.rs`). Before it only the
   named, closure and method paths shadowed `$!` per call, with an env
   insert, so a sub with parameters (the light paths) read its caller's
   `$!`; the owning frame root now answers for every path. A block that
   touches `$!` (a read, a `try` / `CATCH`: `CompiledCode::uses_routine_state`
   and `sets_routine_state`, renamed from the `match_state` pair) shares its
   scope's `$!` through a cell, and the `try` / `CATCH` writers store through
   it (`Env::assign_error_var`), so `t { try { die } }` sets the `$!` of the
   scope the block is written in. The per-call `$!` resets on the named,
   closure and method paths are now redundant on a frame with a root and
   are left for phase 4, which removes the call-setup env work.
   Release-build Ir against the merge base (interpreter lane / JIT lane):
   `bench-fib` +0.01% / +0.02%, `bench-tak` +0.01% / +0.02%, `method-call`
   +0.09% / +0.10%, `poly-call` +0.02% / +0.04%, `bench-ctor` +0.20% /
   +0.10%, `bench-multi-dispatch` +0.13% / +0.23%, `bench-string` +0.72% /
   +0.26% (most of it allocator layout and the base binary's directory; the
   setter itself moved ~70k Ir into its shared helper).
   **Slice 12 done** (`fix/12529-trir-routine-owned-implicits`): the first
   step of the `$_` family. TRIR compiled a routine's `$_`, `$/`, `$0` and
   `$<name>` reads as free variables, resolved by name through the running
   env, and TRIR pushes no frame root, so `sub f($x) { $_ }` called inside
   `given 'x' { }` answered `'x'` and a sub with parameters read its
   caller's `$/`. The VM call paths already owned them (the per-call `$_`
   reset, slices 10 and 11); only TRIR did not. TRIR now declines a read of
   a routine-owned implicit, as it already declined `$!` and an assignment
   to `$_`. No benchmark's TRIR accept/decline set changed. The per-call
   `$_` resets on the VM paths remain; moving `$_` onto slice 10's owner
   walk is the next step of the family.
   **Slice 13 done** (`perf/12529-routine-owned-topic`): `$_` on slice
   10's owner walk. A new symbol flag, `ROUTINE_OWNED` (the
   `ROUTINE_SCOPED_IMPLICIT` names plus `_`), selects the walk. The topic
   stays out of `ROUTINE_SCOPED_IMPLICIT` because the return merges and
   `uses_routine_state` treat it on their own terms. An owning frame root
   answers `Any` for a `$_` the routine has not bound. A block's frame root
   answers from its capture. A capture made in a routine that had not set
   its `$_` holds that routine's `Any`, not the program scope's topic
   (`layered_capture` and `filtered_flat_capture`).

   - The per-call `$_` inserts on rooted frames are gone: named, light,
     typed-light, fast, closure-dispatch and both method paths. So are the
     redundant `$!` resets on the named, closure and method paths. Of 12
     per-call sites, four remain, all for a method that creates closures
     and so runs unrooted (#12882).
   - The fast path gives a topic-reading routine a root. `reads_topic` now
     folds in nested blocks, as `uses_routine_state` does, so a block
     created in the body cannot read the caller's topic. A light frame is
     reused for such a body only when that frame owns its state.
   - A placeholder block or a WhateverCode that reads `$_` binds its
     caller's in its own frame. It runs as a routine, but its topic is the
     outer one.
   - An empty routine frame is now common, because nothing is inserted.
     That exposed two "an empty tier is invisible" shortcuts. `scoped_child`
     skipped an empty owner root, which a lookup must stop at. The
     flattening short-circuit replaced the frame's `FrameIds` with its
     parent's, which made a block's `return` lose its target routine. An
     owner root is no longer skipped, and the short-circuit keeps the
     frame's ids.

   Release-build Ir against the merge base (interpreter lane / JIT lane):
   `bench-fib` +0.06% / +0.11%, `bench-tak` +0.04% / +0.06%, `method-call`
   -2.32% / -2.36%, `poly-call` -0.74% / -0.69%, `bench-ctor` -1.14% /
   -1.05%, `bench-multi-dispatch` -1.14% / -1.16%, `bench-string` -0.05% /
   +0.03%. Allocations: -10,000 on `method-call` and `bench-multi-dispatch`
   (one overlay map per call that no longer receives a `$_`).

   **Slice 14 done** (`fix/13595-reflective-closure-block-lexicals`,
   #13595): `::('$x')` and `::($name)` count as naming a lexical by string.
   They compile to `IndirectTypeLookup`, which `scan_reflective_name_access`
   did not list. A program whose only reflective access was such a lookup
   therefore never set the reflective-access flag. Its scalar stores then
   skipped the env mirror (ADR-0039 §16), and no closure got a reflective
   capture. The result was `my $s = 1; say ::('$s')` dying with "No such
   symbol". The same held for a closure reading its exited block's lexical
   this way. Only a constant name without a sigil (`::('Int')`,
   `::('Foo::Bar')`) stays non-reflective: it names a type, and the symbol
   table answers it.

   Two shapes of #13595 remain:
   - A block-local lexical that shadows a program-scope one of the same
     name: a program-scope capture is consulted after the live program
     scope.
   - A sub declared in a closure body that creates the closure: that sub's
     frame is not linked to the closure it was declared in.

   **Slice 15 done** (`fix/13595-closure-declared-sub-link`, #13595): both
   shapes.

   - **A sub declared in a closure body.** When such a sub names lexicals by
     string, its declaring code now counts as doing so too
     (`Compiler::note_routines_naming_lexicals`, which checks the routines
     compiled in the body). The closure then gets a reflective capture of its
     creation site. The sub has no static link of its own and resolves
     through the closure's frame, so the capture is where it finds the
     closure's outer lexicals. Before, the closure took only its free
     variables, and the lookup died with "No such symbol".
   - **A nested block's lexicals at program scope.** A program-scope capture
     still links the closure's frame to the live program scope. The nested
     blocks it was created in are not part of that scope, though. Their plain
     lexicals (`Env::block_lexicals_above_program_scope`) are now boxed into
     cells and recorded on the capture (`program_scope_block_names`). The
     link answers them from the capture ahead of the program scope
     (`StaticLink::UnitOuter::capture_names`).

     So `my $s = 1; my $k = do { my $s = 5; -> { ::('$s') } }` reads 5, and
     a write the block makes after creating the closure is seen. A
     program-scope lexical is still read live: `$x = 2` after the closure
     was created is what the closure sees.

   **Slice 16 done** (`fix/12529-class-body-scoped-class-var`): the first
   step toward deleting the return merge, and a census of what it still
   carries. The census instrumented the named path's merge loop and the
   light paths' `finish_positional_light_env`, and logged every key either
   wrote back with a binding that differed from the caller's. It ran over
   every `t/` file and every whitelisted roast file, 730 files in all.
   - `?CLASS` dominated: 599 files, all on the named path. A class body
     bound `?CLASS` in the enclosing env and never unbound it. So the
     mainline read the last declared class (`::('$?CLASS')` answered `B`
     where rakudo fails). Every call into a frame that saw a different
     leaked value also wrote it back to its caller.

     The class body now restores `?CLASS` as it restores the topic. The
     named merge now skips every `?` contextual, as the light paths
     already did.
   - What is left, by file count:
     - frame metadata `__mutsu_*` in 246 files: callable ids, state keys,
       sigilless alias and readonly markers, `__mutsu_type::`, var-source
       names, block-lexical-sub and module-merge markers. These are
       phase 1's per-frame metadata.
     - type and package names in 142 files (`NativeCall::T`, a nested
       `Test::A`, `=pod`). These are phase 1's symbol-table names.
     - plain user lexicals in 130 files. These are writes that reach a
       caller's lexical by name instead of through a cell. Examples are a
       `does` mixin or a by-name store in a closure the callee runs
       (`lives-ok { $a does R }`), a `CONTROL`/`CATCH` handler's writes,
       and module-scope aggregates a routine mutates.

     Each category has to reach zero, through a cell or a frame field,
     before the merge loops can go.

   **Slice 17 done** (`fix/12529-mainline-capture-sibling-shadow`): a
   mainline sub's free variable stays bound to the declaration it captured
   (ADR-0024) when more than one block shadows the name.

   The first shadowing `if` branch or loop body restored the outer value on
   exit (`pop_loop_local_scope`) into *every* slot of that name. That
   included a later sibling block's slot, which then held the captured cell.
   When that sibling's `my $q` ran, `mainline_capture_at` saw a slot holding
   the cell and took the declaration for a same-scope re-declaration of the
   captured variable (#11263). So it dropped the capture, and from then on the
   sub read its caller's `$q`.

   The check now also requires the slot to be one a sub of the code captured
   (`CompiledSubDeclPlan::free_var_decl_slots`). The by-name restore into
   sibling slots is left as is: the sibling's declaration overwrites the slot.

   **Slice 18 done** (`fix/12529-does-var-is-a-write`): the first entry of
   slice 16's lexical category. `$x does R` (`OpCode::DoesVar` on a
   variable) rebinds `$x` as `=` does, but the free-variable analysis did
   not count it as a write. So a closure that mixed into a captured `$x`
   left `$x` a plain value, and the closure changed only its own copy.

   Only the return merge carried the mixin back, and only when the closure
   ran inside a callee called from the declaring frame
   (`lives-ok { $a does R }`). A sibling closure, a `start` block's caller
   and a routine that outlived the frame kept the unmixed value.
   `op_name_write_const_idx` and `op_name_const_idx` now list a non-bareword
   `DoesVar`, so the variable is boxed into a shared cell and the store goes
   through it.

   **Slice 19 done** (`fix/13604-reflective-sub-captures-visible-lexicals`,
   first half of #13604): §1.3 at program scope. A mainline sub that named a
   lexical by string (`EVAL '$q'`, `::('$q')`), called from a block with its
   own `my $q`, read the block's `$q`. Its static link targets the program
   scope's env, and a block at program scope writes its declarations there.

   Two changes fix it:
   - **Capture.** Such a sub now captures every lexical visible at its
     declaration into its ADR-0024 bucket, not only its free variables. The
     compiler bakes them into `free_var_decl_slots` when a body has
     `static_name_lookup`, and the registration captures them. Names the
     body declares itself (its `my`s and parameters) are left out, because
     the bucket answers ahead of the frame. This applies only to a sub
     declared at the unit's top level. In a nested block, `local_map` at
     the sub's position still names an outer `$x` that the block redeclares
     further down, and that later `$x` is what the string lookup must find
     (roast `S04-declarations/my-6e.t`).
   - **One chokepoint.** `::('$q')` reads and `::('$q') = v` stores now go
     through the same chokepoint as a `$q` read
     (`get_env_with_main_alias`, `unit_lexical_slot`), not the raw env.
     `IndirectTypeLookupStore` now counts as reflective access, as the read
     already did.

   **Slice 20 done** (`fix/13604-unit-outer-hidden-set`, second half of
   #13604): a top-level sub that names lexicals by string no longer sees a
   name declared only in the block that calls it (`if True { my $z }` calling
   `sub { EVAL '$z' }`), nor a variable a block-scoped `use` imported. The
   program scope binds both while the block runs, and the static link used to
   answer with them.

   - **Block-only lexicals.** The unit's compiler records the names its nested
     blocks declare (`pop_local_scope`). When the unit finishes, each
     top-level routine with `static_name_lookup` gets the ones the top level
     does not also declare (`CompiledCode::unit_hidden_lexicals`). The set is
     recorded even when empty, because it also marks the routine as one whose
     lexical outer is the unit's top level.
   - **Block imports.** At call time the frame adds the plain lexicals the open
     block-scoped import scopes bound (`imported_env_keys`). A key that
     shadows an enclosing import's binding stays visible: the routine does see
     the outer binding, and the block's value is what the link still finds.
   - **The hide.** The marked frame root carries the union
     (`Env::set_unit_hidden`, a field of the root's own overlay). A lookup
     that would continue at the program scope for one of those names stops
     there (`Env::static_hides`), and the static view drops them too. A light
     call does not reuse its caller's empty unit frame for such a routine,
     since the hide needs a root of its own.

   **Slice 21 done** (`fix/12529-merge-census`): a second census of the
   lexical category, and one fix from it. The census logged every plain
   user lexical the named and light return merges carried back with a
   different binding, over every `t/` file and every whitelisted roast file:
   83 files. A second run of those 83 with the merge of plain lexicals
   switched off showed which ones actually depend on it: 26. By shape:
   - **A sub-signature's defaults and `where` constraints** (`*@ ($x where
     { $c++ })`, `[$a, $b = $x]`). The binder evaluates them from the AST at
     call time, and `fold_decl_time_param_captures` folded only top-level
     parameters' expressions into the routine's free variables. So a
     sub-signature's expression resolved its outer lexicals through the
     caller chain: it read a caller's same-named lexical, and its writes
     reached the declaration only through the merge. Fixed: the fold
     descends into `sub_signature` and `outer_sub_signature`.
   - **A closure literal as a user infix operand** (`{ $n++ } xxx 3`,
     `t/control/bare-block-infix-operand.t`). `compile_infix_func` compiled
     the operands in a non-escaping position, unlike a call argument
     (`is_closure_literal_arg`), so a lexical the closure writes stays a
     copy. Not visible while the merge runs; the fix (pass
     `is_closure_literal_arg` as the operands' `escaping`) goes with the
     slice that drops the merge.
   - **A resuming `CATCH` handler run at the throw site** (ADR-0072,
     `t/exceptions/catch-resume-across-jit-frame.t`): its write to a lexical
     of the installing block (`$caught = True`) is lost.
     `leave_installing_frame` writes the installing frame's live slot and the
     throw site's env, and only the merge carries the env write past the
     callee frames. CONTROL handlers (`control-warn-handler-write-through-
     aggregate-arg-call.t`) have the same shape.
   - **Two sibling blocks sharing a slot** (`multi_scope_slots`): a sub each
     block declares over the same name (`{ my $a; sub foo { $a++ } } { my $a
     = 3; sub bar { $a++ } }`, then another statement) gets no cell, because
     the block arm declines to box a slot two scopes own, and its writes
     reach the block only through the merge
     (`t/routines/call/hoisted-sub-captures-later-my.t`).
   - **Binding** (`:=` aliases and rebinds that reach a caller,
     `t/vm/binding/bind-alias-reverse-write.t`,
     `bind-source-tracks-through-call-chain.t`,
     `t/vm/scope/bind-rebind-outer-lexical-leaves-earlier-alias.t`,
     `closure-rebind-reaches-declaring-frame-through-linked-callee.t`).
   - The rest, one or two files each: `cas` on a typed aggregate, a
     container-trait cell (`BagHash` `.values`), `substr-rw` bound in a
     routine, a regex declaration's `s///` on an outer string, a role body's
     composition-time write, a `QuantHash` rebind, a BEGIN-lifted type body,
     JSON::Tiny's grammar actions, and roast `S02-names-vars/
     variables-and-packages.t`, `S02-types/array-shapes.t`,
     `S04-blocks-and-statements/pointy-rw.t`, `S04-phasers/in-eval.t`,
     `S32-hash/kv.t`.

   **Slice 22 done** (`fix/12529-resume-handler-writes`): the resuming
   `CATCH`/`CONTROL` entry of slice 21's census. An inline handler
   (ADR-0072) now writes the installing block's lexicals into that block's
   live slots and nowhere else, so the return merge no longer carries them.
   The shared machinery moved to `src/runtime/handler_frame.rs`.

   - **The drain clobbered the slot.** `leave_installing_frame` already
     wrote the live slot, but it also queued the name for the call site's
     drain (`pending_rw_writeback_sources` and kin). The drain reads the
     installing frame's env, which only the merge had updated, so with the
     merge off it put the old value back. A slot written live is no longer
     queued.
   - **CONTROL records its installing activation** (`ControlHandlerEntry::
     installing_base`/`installing_call_depth`, as CATCH did). It seeds and
     flushes the live slots, and runs on the installing activation's env
     when its call frame has it (`enter_handler_env`).
   - **No write to the throw site's env.** A write to one of the handler's
     own locals no longer lands in the env of the frame that threw. A
     same-named lexical of that frame or of one in between took it before:
     `sub w { my $m = 'callee'; warn 'W'; $m }` returned `W`, and the
     handler's own `$m` stayed empty once the raise site had its own `$m`.
     When the installing activation's env is not reachable (it made a light
     call, which keeps no call frame), the handler runs on a private overlay
     of the throw site's env, seeded with the installing slots' values for
     its by-name reads. A dynamic, or a lexical captured from further out,
     still goes to the throw site.
   - **The carrier log.** `warn` and `EVAL` are carrier calls, which log
     by-name writes and reconcile the caller's slots from env afterwards.
     The handler's writes are not the throw site's, so the log is set aside
     while it runs (`suspend_carrier_for_handler`). Only what is written back
     to the throw site's env is logged.

   Pinned by `t/exceptions/catch-control-inline-handler-writes.t`. Still
   open: the light call paths keep no record of their caller's env, so a
   handler installed by a light caller resolves its free names through the
   caller chain with static links suppressed.

   **Slice 23 done** (`fix/12529-sibling-slot-cells`): the sibling-slot
   entry of slice 21's census. Sibling blocks that each declare `my $a`
   share one slot (`multi_scope_slots`), and ADR-0024's block capture
   declined to box such a slot at all, because a cell left there by the
   first block would fuse its `$a` with the next block's. So a sub over the
   second block's `$a` wrote it only by name, and only the return merge
   carried the write to the block.

   The decline now covers only what the slot holds when it may be another
   scope's leftover. Two bindings are known to be this scope entry's own:
   - the hoisted pass's seed (#9911), a fresh cell the scope's declaration
     adopts;
   - in sequence, a slot the scope's own `my` has already filled. The
     compiler records those as `CompiledSubDeclPlan::own_scope_decl_slots`
     on the in-sequence plan, the same set `hoist_seed_slots` is on the
     hoisted one.

   Pinned by `t/routines/call/sibling-block-subs-capture-own-cell.t`.
   Slice 23 also took the first two binding files of slice 21's census off
   the merge (`bind-alias-reverse-write.t`,
   `bind-source-tracks-through-call-chain.t`): their sibling blocks each
   declare the bound names.

   **Slice 24 done** (`fix/12529-bind-through-cells`): the module half of
   the binding entry of slice 21's census
   (`bind-rebind-outer-lexical-leaves-earlier-alias.t` 14-17). A module file
   with no `unit` declarator (Rakudo's `Telemetry.rakumod` shape) runs its
   body in the loader's env, and its file-scope lexicals stayed there:
   only a `unit` file had them moved into `unit_lexicals`, under its
   package. Its routines run in `GLOBAL`, so they read those names through
   the caller chain. A caller's own same-named lexical answered instead
   (`sub c { my $count = 'caller'; module-count() }`), a rebind inside one
   routine reached its siblings only through the return merge, and the
   importer's `EVAL '$x'` saw the module's variable.

   - **The bucket.** Such a file's file-scope lexicals now move into
     `unit_lexicals` too, keyed by the compunit itself (its source path,
     which no package name collides with), and the compunit is recorded in
     `LexicalState::compunit_lexical_units`.
   - **The lookup.** `unit_lexical_slot` and its `_mut` twin, and the
     `@`/`%` type lanes, find the bucket by the file the running routine was
     declared in, walking past inlined block frames
     (`running_compunit_lexical_unit`, `src/vm/vm_compunit_lexicals.rs`).
     TRIR pushes no routine frame, so its outer-binding seed asks by the
     chunk's `def_file` (`compunit_lexical_of_file`).
   - **The `GetGlobal` scalar shortcut** read `env` directly for any routine
     running in `GLOBAL`. A `unit` module's routine never qualified (it runs
     in its own package); a package-less module's does, so the shortcut now
     also steps aside for a name the running compunit's bucket holds.

   Pinned by `t/modules/package-less-module-file-scope-lexicals.t`.
   Remaining in the binding entry: a rebind in a block handed to a routine
   (`closure-rebind-reaches-declaring-frame-through-linked-callee.t`).

   **Slice 25 done** (`fix/12529-closure-rebind-cell`): the last binding
   file of slice 21's census
   (`closure-rebind-reaches-declaring-frame-through-linked-callee.t`). A
   closure that rebinds an `@`/`%` of the frame that created it
   (`run-it({ @y := [3] })`) rebound only its own env entry: a scalar the
   closure rebinds gets a binding cell at the capture (#9307), but a
   container takes its cell at the declaration, and the declaration asked
   for one only when an escaping closure captured it. So the return merge
   of `run-it` was what carried `[3]` to the block.

   - The compiler now adds an own `@`/`%` a nested closure rebinds to the
     declaration-site container lane (`needs_cell_unvouched_containers`),
     escaping or not.
   - That declaration's cell is a binding cell when the slot is rebound
     (`box_decl_local_container_cell`), so a sibling closure made before the
     rebind shares the binding too, and a name bound to the old container
     keeps it. The capture wraps a plain cell it still finds
     (`bind_rebound_captured_containers`).

   Pinned by `t/routines/closure/closure-rebinds-captured-container-through-cell.t`.

   **Slice 26 done** (`fix/12529-infix-closure-operand-escapes`): the
   infix-operand entry of slice 21's census, and a third census.
   `compile_expr_infix_func` compiled a closure literal operand of a user
   operator (`{ $n++ } xxx 3`) in a non-escaping position, so a lexical it
   writes stayed a copy and only the call's return merge carried the write.
   It now passes `is_closure_literal_arg` as the operand's `escaping`, as a
   call argument does; an operator that keeps the closure and runs it after
   the statement is pinned too
   (`t/routines/closure/user-infix-closure-operand-escapes.t`).

   The third census reran slice 21's method (the merge of plain user
   lexicals switched off in both the named and the light return merges,
   every `t/` file and every whitelisted roast file, on top of slices
   22-26). Twelve files still depend on the merge, down from 26:
   - `S04-blocks-and-statements/pointy-rw.t` 8 and `S32-hash/kv.t` 27:
     the aliases `$pair.values` / `$pair.kv` hand out must stay `rw`.
   - `S04-phasers/in-eval.t` (10 tests): phasers inside `EVAL`.
   - `t/control/begin-lift-names-inner-type-with-body.t`,
     `t/oo/role/role-body-composition-timing.t`: a BEGIN-lifted type body
     and a role body's composition-time write.
   - `t/regex/regex-decl-self-referential-slash.t`: a regex declaration's
     `s///` on an outer string.
   - `t/routines/closure/closure-capture-keeps-container-traits.t`,
     `t/routines/signature/quanthash-parameterized.t`: a container-trait
     cell and a `QuantHash` rebind.
   - `t/types/cas-typed-constraint.t`: `cas` on a typed aggregate.
   - `t/vm/codegen/adr0112-trir-caller-frames.t`: TRIR's caller frames.
   - `t/vm/underscore-kebab-var.t`: the `_`/`-` spelling alias of a name.
   - `t/vm/writeback/container-bind-free-var-aliases-source.t`: `:=` of a
     free container variable to a source.

   The temporary instrument is a two-line gate in each merge loop (skip a
   key that is a plain user lexical when `MUTSU_NO_LEXMERGE` is set); it is
   not committed.

   **Slice 27 done** (`fix/12529-pair-values-rw-alias`): the first entry of
   slice 26's census (`pointy-rw.t` 8, `S32-hash/kv.t` 27). `k => my $ = v`
   built a Pair whose value was a `WrapScalar` item, not the anonymous
   scalar's container, so the value was not writable at all
   (`(a => my $ = 42).value = 5` died "Cannot modify an immutable Any";
   rakudo assigns). An `is rw` alias over `.value`/`.kv` then wrote a
   rebuilt Pair back by name into the closure's env, which only the return
   merge carried to the frame that held `$pair`.

   Such a Pair now holds the container: the FatArrow compile flags its
   value (`Compiler::anon_scalar_as_cell`, `is_untyped_anon_scalar_decl`),
   and the anonymous declaration emits a `Mu`-typed `WrapTypedContainer`
   cell in place of `WrapScalar`. Both `WrapScalar` and that cell store the
   default (`Any`) for a `Nil` initializer, as an untyped `my $` does.
   Pinned by `t/collections/range-pair/pair-value-anon-scalar-container.t`.

   **Slice 28 done** (`fix/12529-eval-closure-cells`): the
   `S04-phasers/in-eval.t` entry of slice 26's census. An `EVAL`'d unit is
   compiled on its own, so a lexical of the code that called `EVAL` is not
   one of its locals: it reads and writes the name on the env it runs on,
   which is the caller's. A closure the unit created captured the plain
   value from that env, so its write (`EVAL '$handle = { $h ~= "x" }'`,
   later run by `lives-ok`) reached `$h` only through the return merge of
   the routine that ran it.

   Creating such a closure now promotes each name it writes that the
   creating code has no slot for, and that the env binds to a plain user
   lexical, to a shared cell in that env, inside an `EVAL` only
   (`cell_eval_captured_caller_lexicals`, `src/vm/vm_eval_capture_cells.rs`).
   It logs the name for the `EVAL` carrier, whose call site moves the cell
   into the caller's slot (`writeback_carrier_writes`), so the caller, the
   closure and its siblings share one container. Pinned by
   `t/routines/closure/eval-closure-writes-caller-lexical-cell.t`.

   **Slice 29 done** (`fix/12529-quanthash-rebind-cell`): the QuantHash
   rebind entry of slice 26's census
   (`t/routines/signature/quanthash-parameterized.t` 25). Slice 25 gave a
   rebound `@`/`%` its binding cell at the declaration, but
   `box_decl_local_container_cell` boxes only an Array or Hash value, so a
   variable bound to an immutable Set, Bag, Map or List (`my %qh :=
   Set.new(1, 2)`) and rebound in a closure still rebound only the
   closure's copy. A rebound slot now takes its binding cell whatever it
   holds, except a mutable QuantHash: its `.values`/`.kv` write-back stores
   the new weights by name and does not see through a binding cell yet
   (roast `S02-types/quanthash.t` 97), so a closure's rebind of a `%`
   bound to a SetHash/BagHash/MixHash still depends on the merge. Pinned by
   `t/routines/closure/closure-rebinds-captured-immutable-container.t`.

   Correction to slice 26's census: `t/vm/underscore-kebab-var.t` is not a
   dependent. It compares `$*EXECUTABLE-NAME` with `mutsu`, and the census
   binary had a different file name.

   **Slice 30 done** (`fix/12529-quanthash-values-writeback`): the
   container-trait entry of slice 26's census
   (`t/routines/closure/closure-capture-keeps-container-traits.t` 21). A
   `for` over a `$`-held mutable QuantHash's `.values`/`.kv`/`.pairs`
   (`$_ = 7 for $b.values`) writes the updated container back to `$b` by
   name, but the free-variable analysis did not count that as a write. So
   a closure looping over a captured `$b` left it a plain value, and only
   the return merge of the routine that ran the closure carried the write.
   The loop's `TagContainerRef` of a `$` name now counts as a write
   (`op_name_write_const_idx`), so the capture boxes `$b` into a cell and
   the write-back goes through it. Pinned by
   `t/routines/closure/quanthash-values-loop-in-closure-writes-back.t`.

   **Slice 31 done** (`fix/12529-cas-code-form-cell`): the
   `t/types/cas-typed-constraint.t` entry of slice 26's census (11). The
   first `cas` on an element of an `@`/`%` variable moves the variable
   onto a celled atomic store, and `init_celled_atomic_store` (and
   `celled_hash_elem` for a new key) stored that container into the
   running code's env entry by name. When the first `cas` ran inside a
   closure (`throws-like { cas(@c[0], ...) }`), only the closure's copy of
   `@c` moved, so the declaring block kept the uncelled array whose
   elements later `cas` calls no longer write; the merge of the routine
   that ran the closure carried the move. A captured `@`/`%` is bound
   through its shared cell (ADR-0039), so the celled store now goes into
   that cell. Pinned by
   `t/routines/closure/cas-in-closure-celled-store-reaches-declarer.t`.

   **Slice 32 done** (`fix/12529-bind-adopts-source-type`): the
   `t/vm/writeback/container-bind-free-var-aliases-source.t` entry of
   slice 26's census (16-17). A named sub's capture of an outer lexical
   gives it a cell at the sub's registration (ADR-0024), but
   `type_constrained_unboxable` kept every name with a non-native type
   constraint out of it. That rule is about a `$` variable, whose
   constraint is re-checked by name on each store; an `@`/`%` name's
   constraint is its element type, checked by every element store whether
   the container sits in a cell or not (a closure's typed container already
   takes one at its declaration). So `sub f { @declared := @src }` with a
   `my Cool @declared` rebound only the sub's own entry. An `@`/`%` name
   now always takes the cell (a native element type such as JSON::Fast's
   `my str @out` already did, as a value type). Pinned by
   `t/types/named-sub-rebinds-typed-outer-container.t` and
   `t/modules/module-nested-sub-captures-routine-array.t`.

   A fourth census (slice 21's method on top of slices 27-32: the merge of
   every plain user lexical, sigil-less scalar keys included, switched off
   in both return merges; every `t/` file and every whitelisted roast file)
   leaves four `t/` files and no roast file depending on the merge:
   `t/regex/regex-decl-self-referential-slash.t` (12),
   `t/control/begin-lift-names-inner-type-with-body.t` (2-3),
   `t/oo/role/role-body-composition-timing.t` (6-7, 20) and
   `t/vm/codegen/adr0112-trir-caller-frames.t` (1).
   `t/vm/underscore-kebab-var.t` is slice 29's false positive; the roast
   files that timed out under the parallel debug run pass with the merge off
   when run alone.

   **Slice 33 done** (`fix/12529-smartmatch-subst-cell`): the
   `regex-decl-self-referential-slash.t` entry of the fourth census.
   `$x ~~ s///` (and `tr///`) writes the modified topic back to `$x` by
   name (`SmartMatchLhs::Var`), but the free-variable analysis did not
   count that as a write of `$x`, so a closure doing it read `$x` as a
   snapshot upvalue, and the write-back replaced the closure's own env
   entry. The analysis now counts a smartmatch whose right-hand side holds
   a `Subst`/`Transliterate` as a write of its left-hand variable
   (`smartmatch_writeback_name`), and the write-back stores into the
   variable's cell when it has one. Pinned by
   `t/regex/subst/closure-substitution-on-captured-var-writes-through-cell.t`.

   **Slice 34 done** (`fix/12529-role-body-outer-cells`): the
   `t/oo/role/role-body-composition-timing.t` entry of the fourth census
   (see slice 33). A role's deferred body runs at composition, in whatever
   frame composes the role, and reached the variables it does not declare
   by name: `role R { $count++ }` composed inside a sub wrote the sub's
   env, and only the sub's return merge carried the write to the declaring
   block (a composer's own `my $count` took the write instead). #11087
   already gave a body `:=` source a declaration-site cell
   (`RoleDef::body_bind_cells`); the role plan now gives one to every
   outer lexical the deferred body names (`role_body_outer_ref_slots`, an
   `ast_visit::Visit` over the deferred statements), so each run of the
   body reads and writes the declaration site's variable. Pinned by
   `t/oo/role/role-body-writes-declaration-site-lexicals.t`.

   **Slice 35 done** (`fix/12529-class-body-free-vars`): the
   `t/control/begin-lift-names-inner-type-with-body.t` entry of the fourth
   census. A `my class` declared inside a routine runs its body statements
   when the routine runs, each as a precompiled chunk of its own
   (`ClassBodyOp`), so the routine's op scan never saw the outer lexicals
   a body names: `sub f { my class K { $log ~= 'body;' } }` did not
   capture `$log`, the chunk reached it by name through the caller chain,
   and only `f`'s return merge carried the write. `compute_free_vars` now
   folds the class-body chunks' free variables and free writes into the
   declaring code's (`class_body_chunk_codes`), as it folds a nested
   closure's. Pinned by
   `t/oo/class/class-body-in-routine-writes-outer-lexical.t`.

   **Slice 36 done** (`fix/12529-light-runtime-name-writes`): the
   `t/vm/codegen/adr0112-trir-caller-frames.t` entry of the fourth census,
   its last. A write whose target name exists only at run time
   (`CALLER::CALLER::.BIND-KEY($name, ...)`, `$::($n) = v`) lands in the
   writing frame's env and is recorded in `pending_runtime_name_writes`.
   The named return path carries those into the caller env
   (`propagate_pending_caller_writes`); the light paths had no such step
   and relied on their plain-lexical merge. Both light paths now read the
   pending writes before their merge and store them into the restored
   caller env after it (`vm_call_light_runtime_names.rs`). Pinned by
   `t/vm/frames/light-call-carries-runtime-name-writes.t`.

   **Slice 37 done** (`refactor/12529-drop-plain-lexical-merge`): the
   plain-lexical return merge is gone. With slices 33-36 merged, a run of
   every `t/` file with the merge of plain user lexicals switched off left
   no dependent (the fourth census's roast run had none already), so the
   named merge loop (`call_compiled_function_named_inner`) and both light
   merges (`merge_positional_light_env`, `merge_light_env`) now skip every
   key `symbol::flags::PLAIN_USER_LEXICAL` marks, permanently. A callee's
   write to an outer lexical reaches the declaring frame through the
   variable's shared cell, and a runtime-named write through
   `propagate_pending_caller_writes` (named) or
   `vm_call_light_runtime_names.rs` (light). What the merges still carry
   is system names and metadata (`__mutsu_*`, `$!`-style implicits handled
   per their own rules, dynamics, type names), the subject of phase 1's
   per-frame metadata work and of phase 4. Pinned by
   `t/routines/call/return-merge-skips-plain-lexicals.t`.

   A fifth census, after slice 37, logged every key the named and light
   return merges still write back with a binding that differs from the
   caller's, over every `t/` file and every whitelisted roast file: 3351
   writes in 382 files. By file count:
   - type names (`NativeCall::T`, `Test::A`, a `my class` the callee
     declared): 122 files;
   - frame metadata: `__mutsu_callable_id` 39, `__mutsu_for_chunk` 36,
     `__mutsu_sigilless_alias` 25, `__mutsu_block_lexical_sub` 23,
     `__mutsu_scalar_bind_no_container` 22, `__mutsu_sigilless_readonly`
     21, `__mutsu_module_merge` 19, `__mutsu_type` 18, `__mutsu_state_key`
     17, `__mutsu_rw_map_topic__` 16, `__mutsu_var_source_name` 15,
     `__mutsu_var_meta` 13, `__mutsu_lexical_dynamic` 13,
     `__mutsu_constant_var` 10, `__mutsu_bound_decont` 9, and a tail of
     single-digit keys;
   - dynamic variables (`*REPO`, `*CRO-UNHANDLED-ERROR-REPORTER`): 24;
   - Pod blocks (`=pod`): 21;
   - compiler temporaries (`@__destructure_tmp__`, `__begin_value_N`,
     placeholders `^a`) and `&?BLOCK`: a handful each.

   These are phase 1's per-frame metadata (ADR-0084, #7817) and the type
   and dynamic names that §2 moves to the symbol table and the call-frame
   stack. Each category leaves the merge when its owner resolves it
   without the caller chain; the merge and the `scoped_child(caller)`
   overlay go when the list is empty.

   **Slice 38 done** (`refactor/12529-drop-type-name-merge`): the type-name
   row of the fifth census. A census that also skipped every key spelled
   as an identifier starting with an uppercase ASCII letter (optionally
   `::`-qualified: a type name, a package-qualified variable such as
   `$GLOBAL::n`'s key `GLOBAL::n`, an uppercase-named scalar such as `my
   $F`'s key `F`) found one dependent in `t/` and none in roast:
   `t/oo/role/role-body-qualified-incdec-from-routine.t`, whose second call
   of `sub body { my class K { $GLOBAL::k1++ } }` read the copy the first
   call left in the env instead of the package store (`our_vars`) the `++`
   had written. A package-qualified scalar read (`GetGlobal`, fast path
   included) now consults `our_vars` before the env. The merges skip the
   new `symbol::flags::UPPER_IDENT_KEY` with `PLAIN_USER_LEXICAL`. Pinned
   by `t/routines/call/return-merge-skips-uppercase-keys.t`.

   A sixth census switched the named and light return merges off
   entirely, on top of slice 38. Nine `t/` files and two roast files
   depended on them (`t/vm/underscore-kebab-var.t` is slice 29's false
   positive): the dynamic-variable writes below, and
   `t/vm/binding/bind-rebind-alias-leaves-source.t`,
   `t/vm/scope/nqp-lexical-outer-ops.t`,
   `t/vm/scope/dynamic-var-bound-missing-elem-push-autoviv.t` (a callee's
   `$*t :=` rebind), `roast/S02-names/is_dynamic.t` and
   `roast/APPENDICES/A01-limits/misc.t`.

   **Slice 39 done** (`fix/12529-dynamic-var-decl-cell`): a callee's write
   to a caller-declared dynamic (`sub w { $*x = 5 }` called inside `my
   $*x = 99`) reached the declaring frame only through the merge. A
   dynamic declaration (`CompiledCode::dynamic_declared_sym`) now takes a
   shared cell when it is stored, like a lexical a named sub captures, so
   the callee's by-name write goes through it. The built-in readers of a
   dynamic (`$*TZ`, `$*SCHEDULER`, `$*PROGRAM-NAME`, `$*SPEC`, `$*REPO`,
   `$*COLLATION`, ..., and `get_dynamic_var`) dereference the cell. With
   the merges off, this takes `closure-dynamic-var-restored-writeback.t`,
   `dynamic-var-not-found.t`, `module-load-writes-caller-dynamic.t`,
   `dynamic-var-writeback-coherence.t`, `regex-literal-is-a-closure.t`,
   `whatever-rw-and-currying.t` and `A01-limits/misc.t` off the list. A rebind of a caller's dynamic (`$*t := ...`) still needs the
   merge: a binding cell at the declaration broke the rebind itself and
   was left out. Pinned by
   `t/vm/scope/dynamic-var-written-by-callee-through-cell.t`.

   **Slice 40 done** (`fix/12529-dynamic-rebind-binding-cell`): a
   callee's rebind of a caller-declared dynamic (`$*t := $*root`, or
   `nqp::bindlexdyn('$*q', 8)`) is now a runtime-name write, carried
   across each frame exit to the frame that declares it, as a symbolic
   store to a plain lexical already was (`record_runtime_name_write`,
   `propagate_pending_caller_writes`). The record is made at the op sites
   (`SetGlobal`, the assign-expression op, `bindlexdyn`) and only for a
   dynamic the running code does not declare itself; the declaring frame
   drops the carried name on its own return. Recording it at the env
   store chokepoint instead caught the declaring frame's own `my @*x`
   store, so a later call's fresh `@*x` started with the earlier call's
   contents. A seventh census, on this slice with the merges off, found no
   `t/` or roast file depending on them beyond the slice 29 false
   positive and the container's environment-only failures, so the merge
   loops can go. Pinned by `t/vm/scope/dynamic-var-rebound-by-callee.t`.
   Found on the way and filed as #13701: a callee's write to an `is rw`
   dynamic parameter is lost when the unit holds `EVAL` or an
   `nqp::bindlex*` anywhere (not caused by this slice).

   **Slice 41 done** (`fix/12529-delete-return-merge-loops`): the return
   merges are gone. The named path no longer walks the callee env, and
   the two light-path merges (`merge_positional_light_env`,
   `merge_light_env`) are replaced by one `finish_light_env` that
   restores the caller env (or, for a reused frame, drops what the call
   wrote) and re-inserts only the carried runtime-named writes. With them
   went `CompiledFunction::is_own_enum_key_sym`, whose only use was to
   keep a `my enum` out of the merge. What remains of the copy-back is the
   named path's `free_var_writes` loop, which only inserts a free variable
   the caller env does not have yet (a hoisted sub called before the
   `my` it closes over has run); the censuses left it on. Pinned by
   `t/routines/call/return-copies-no-callee-binding.t`. Found on the way
   and filed as #13705: a routine's `my &*cb` still leaks to its caller,
   through some store other than the merge.

   An eighth census logged every non-dynamic key a by-name lookup found
   in an intermediate caller frame (below the running frame's root, above
   the program scope, and not answered by the frame's own capture). Most
   hits are runtime bookkeeping rather than user name resolution: the
   speculative `__mutsu_type::*` probes (`declared_meta_lane`), `@_` and
   `self` at argument binding, the map/grep inline plan's "caller
   priority" probe, the CATCH handler's env reconcile, and the closure
   call's own copy-back (`call_compiled_closure_in_unit`), which slices
   37-41 had not touched: a third return merge.

   **Slice 42 done** (`fix/12529-closure-writes-through-cells`): a census
   with that closure copy-back switched off found fifteen dependents in
   `t/`. Eleven were a non-escaping closure (a statement-modifier `for`'s
   pointy block, a pointy `if`, a block passed to a routine, a CONTROL
   handler's block) writing a captured local: the escape analysis gave a
   cell only to a local an *escaping* closure writes, and left the rest
   to the copy-back. Every closure that writes a capture now earns it the
   cell (`needs_cell` no longer requires `escapes`, and a write to an
   outer frame's variable bubbles up as `needs_cell_free_vars` either
   way). One more was `push_to_shared_var`: once a thread had run, a push
   to a twigil'd array (`@*LOG`) dropped the running frame's binding even
   when no shared entry held the name, and pushed onto a detached copy
   stored by name in the closure's frame; it now takes that branch only
   for a name the shared store holds. Left for the next slice: a closure
   created while a grammar subrule's argument runs (`<x({ $calls++ }())>`),
   whose creating code is compiled at match time, so no frame knows to
   give `$calls` a cell. Pinned by
   `t/routines/closure/closure-call-writes-through-cell.t` and
   `t/concurrency/concurrent-start-then-dynamic-push.t`.

   **Slice 43 done** (`fix/12529-closure-copyback-free-var-writes`): the
   closure call's copy-back no longer walks the closure's plain bindings.
   It carries back the names in the closure's own `free_var_writes`, under the
   same filters as before (unchanged frees, parameters, the closure's own
   declarations, captures the caller does not have). That narrow carry is
   what the grammar subrule-argument case needs: the closure's creating
   code is compiled at match time, so no frame can give the captured
   variable a cell, and the argument evaluator reads the write back from
   the env the closure returns into. The dynamics in the closure's env are
   carried too: the bundled Cro::HTTP router lost its `@*BIND-FAILS`
   (401 answered as 404) without them, because a push from inside an
   EVAL'd regex assertion rebinds the dynamic to a new array in the
   running frame instead of extending the declared one (#13732); phase 4
   retires that carry with the dynamic chain. With slice 42 in place, a
   re-run of every census dependent passes on this build. Pinned by
   `t/routines/closure/closure-return-carries-only-own-writes.t`.

   **Slice 44 done** (`fix/13732-dynamic-push-in-regex-assertion`): the
   dynamic carry slice 43 kept is gone, so a closure's return carries back
   only its own `free_var_writes`. What it covered was #13732: once a
   thread had started while a frame's `my @*x` was visible, the name-keyed
   shared store held a seed entry for `@*x`, and a later push to `@*x`
   (`push_to_existing_shared_array`, `push_to_shared_var`) went into that
   entry and rebound the running frame's name to it, never the array the
   frame declared. A dynamic is per frame, so the shared store no longer
   stands in for an `@*` name. Pinned by
   `t/concurrency/concurrent-start-then-dynamic-push-from-block.t`.

   **Slice 45 attempted and reverted** (#13756, design in #13757): the
   eighth census's largest group of caller-frame hits was the per-variable
   record probe (`declared_meta_lane` asking for `__mutsu_type::x`), which
   walks past the frame root to a caller's same-named variable. As a result
   a caller's unrelated `my Int @a` fails a routine's EVAL'd push to the
   program's `@a`. Making these records `lexically_scoped` fixed that and
   broke a different shape each time it was narrowed: a module routine's
   own untyped `$desc` reaching a mainline `my Array $desc` through the
   link; a fresh `my $path` reaching a caller's `my URI::Path $path`; a
   destructuring `my (..., $uri)` reaching a `URI:D $uri` record in a
   flattened tier the link reaches. A sigilless parameter's alias mirror
   needs the caller's record, so "the frame binds the name" cannot decide
   either. The env cannot say which variable a record belongs to. A store
   to a local of the running code should take its constraint from the
   code, and only a name the code does not declare should look a record
   up, where the variable resolved. That needs the code threaded to the
   record readers (#13757).

   **Slice 46 done** (`refactor/12529-bind-args-frame-local`): no call's
   return copies anything back any more. A ninth census switched off the
   last two copy-backs, the named path's `free_var_writes` insert-if-missing
   loop and the closure call's `free_var_writes` carry. The only
   dependents were closures created while a grammar subrule's argument runs
   (`<x({ $calls++ }())>`, `<e(-> { $called++ })>`), whose creating code is
   compiled at match time. `eval_block_value_inner` now gives each variable
   such a closure writes (the code's `needs_cell_free_vars`) a cell in the
   env the expression runs over, and the argument evaluator binds the
   caller's variable to that cell. The closure therefore writes the
   caller's variable whether it runs inside the argument or later, as a
   rule argument. Both loops and their bookkeeping (`caller_writeback`,
   `unchanged_free`, `Env::get_sym_through_callers`) are deleted. Pinned by
   `t/routines/closure/subrule-argument-closure-shares-caller-cell.t`.

   **Slice 47 done** (`refactor/12529-binder-probes-read-own-frame`): a
   tenth census logged every non-dynamic key a by-name lookup found in an
   intermediate caller frame, over all of `t/` and the whitelisted roast
   files, with the Rust call site of each key's first hit per file. The
   binder's own probes were the largest rows: `@_` (106,423 hits in `t/`,
   85,266 in roast), because a call with no arguments skipped binding `@_`
   when an `@_` it could see was already empty, and that was usually the
   caller's; and `self` (6,956 in `t/`), which the binder fetched on every
   call to bind an invocant's type capture (`::T $self:`), so a sub called
   from a method found the method's invocant. The `@_` probe now reads the
   running frame only (`Env::get_sym_in_frame`), and the invocant probe
   runs only for a signature that has such a capture. The `t/` census went
   from 234,362 hits to 122,438 (`@_` 282, `self` 1,857). Neither leak
   was observable, because the caller's `@_` is copy-on-write.

   The rest of the list, for the next slices:
   - lexicals of a routine's lexical outer, which is also its caller (a
     `my sub`, Test's `throws-like` closures): correct as they are;
   - a closure call reading its own capture chain
     (`call_compiled_closure_in_unit`'s authoritative and owned captures),
     which phase 4 replaces;
   - `__mutsu_callable_id::*` (`registration_callable_id`: which clone of
     a nested named sub a call reaches);
   - `remove_sym` tombstoning a caller's `?ROLE`, `__mutsu_type::*` or
     sigilless-alias record from the callee frame;
   - the CATCH handler's `leave_installing_env` and the inline map/grep
     loop's topic probe (`enter_inline_loop_env`).
   A tier that a deep chain's flattening copied past a frame root also
   counts as a caller frame here (`@mat` in
   `roast/integration/deep-recursion-initing-native-array.t` is the
   program's own), so the count is an upper bound.

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
| [#12692](https://github.com/tokuhirom/mutsu/issues/12692) | a named `sub` declared in a loop body or `.map` callback closes over the last iteration's lexicals (the per-name stores are keyed by the sub, not the activation) | 3 (slice 9 closes) |
| [#12398](https://github.com/tokuhirom/mutsu/issues/12398) | a routine starts with its caller's `$/` (`m:c` continues from the caller's position) | 3 (slice 10 closes) |
| [#12720](https://github.com/tokuhirom/mutsu/issues/12720) | a `.subst` / `.match` in a block called by another routine does not set the `$/` of the block's scope | 3 (slice 10 closes) |
| [#12902](https://github.com/tokuhirom/mutsu/issues/12902) | `$!` is not routine-owned on the light call paths; a `try` in a block called by another routine does not set its scope's `$!` | 3 (slice 11 closes) |
| [#12850](https://github.com/tokuhirom/mutsu/issues/12850) | a block-declared sub called by name from a closure that outlives the block dies with "Unknown function" | 2 (slice 4 closes) |
| [#12851](https://github.com/tokuhirom/mutsu/issues/12851) | `&g` taken in a closure nested in `g`'s declaring block reads the latest activation's lexicals | 2 (slice 4 closes) |
| [#13037](https://github.com/tokuhirom/mutsu/issues/13037) | a block-declared sub calling a sibling sub of its block by name dies after the block exited | 2 (slice 5 closes) |
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
