# ADR-0024: Mainline is a compunit — named subs resolve mainline free variables through unit-lexical cells, not the ambient env

- Status: Accepted (implementation pending)
- Date: 2026-08-10
- Extends: the `unit_lexicals` mechanism (introduced for `unit module` compunits)
- Related: ADR-0010 (lineage-scoped sharing), ADR-0018 (slot-addressed capture),
  ADR-0023 (binding provenance)
- Resolves: `todo/deep/named-sub-free-variable-reads-are-dynamically-scoped-not-lexical.md`

## Context

A named `sub` declared at mainline whose body reads a free variable (a name it
neither declares nor takes as a parameter) resolves that name **dynamically**
against whatever the calling frame's env currently holds — not **lexically**
against the binding visible at the sub's declaration site. Any block (`{ }`,
`if`, `for`, `start { }`, ...) that declares a same-named `my` shadows the
sub's true lexical binding for every call made from inside that block:

```raku
my $client = "outer";
sub helper($u) { $client }
{
    my $client = "inner";
    say helper(0);   # raku: outer    mutsu: inner
}
say helper(0);       # raku: outer    mutsu: outer
```

Root cause (established in the ticket via `rust-gdb` breakpoints, reconfirmed
on main `4529289aa` 2026-08-10): a free-variable read in a sub body compiles
to `OpCode::GetGlobal` (`src/compiler/expr_helpers.rs:688-695`), and named
subs never receive closure treatment — `exec_register_sub_op`
(`src/vm/vm_register_sub_ops.rs:205-515`) records nothing about what the
sub's free variables meant at its declaration point. At call time the callee
runs in a scoped overlay over the **caller's** env
(`src/vm/vm_call_named_inner.rs:72-75`), so `GetGlobal("client")` falls
through to the caller's flat, name-keyed env — the shadowed value.

### The full divergence matrix (raku-verified 2026-08-10)

`tmp/nsub-lex-matrix.raku` / `tmp/nsub-lex-edge.raku`, raku vs mutsu on main:

| # | Shape | raku | mutsu (bug) |
|---|---|---|---|
| 1 | `my $a=1; sub fa {$a}; $a=2; fa()` — live mutation | `2` | `2` (ok) |
| 2a | sub **writes** its free var while called under a shadow: shadow's value after | `inner` | `set` (shadow clobbered) |
| 2b | ...and the outer lexical after the block | `set` | `outer` (write lost) |
| 3 | closure (`.map({$y})`) created **inside** the sub, called under a shadow | `outer` | `inner` |
| 4 | call after the shadowing block exited | `outer` | `outer` (ok) |
| 5 | two subs sharing one lexical (setter + getter) | `42` | `42` (ok) |
| 6 | for-loop param shadows; sub called from loop body (the Cro shape) | `outer` | loop value |
| 7 | same, but through `start { }` (cross-thread) | `outer` | loop value |
| 8 | call from a block nested inside the shadowing block | `outer` | `inner` |
| adv | closure created **in** the shadow block, passed to and invoked by the sub | `inner` | `inner` (ok — must stay) |

Rows 1/4/5 work today only because the ambient env *happens* to coincide with
the lexical binding when nothing shadows it. Rows 2a/2b show the bug is not
read-only: the write side both clobbers the innocent shadow and loses the
write to the real lexical. Row "adv" is the trap for naive fixes: a closure's
free variables are its *own* captured bindings, and a mechanism that lets the
enclosing sub's frame override them regresses correct behavior.

Also raku-verified: `sub f() { $q }; my $q = 1;` is a **compile error** in
raku ("Variable '$q' is not declared"), so in every valid program the lexical
a sub captures is declared **before** the sub's textual declaration — which is
what makes capture-at-registration (below) see the binding.

### Why this is the missing third mechanism, not a new idea

mutsu already has two "authoritative store consulted BEFORE the ambient env"
mechanisms for exactly this shape of collision:

- **`package_lexicals`** (`src/vm/vm_env_helpers.rs:304-353`): protects a
  `package Foo { my $x; sub f {$x} }` block's `my` from same-named outer
  bindings. Gated on `current_package` being a real (non-GLOBAL) package.
- **`unit_lexicals`** (`vm_env_helpers.rs:356-448`,
  `src/runtime/run_modules.rs:759-841`): protects a `unit module X`'s
  file-scope `my` from the *loading scope's* same-named `my` — the module
  body runs in the caller's env, so both occupy one env key
  (`run_modules.rs:759-764` documents precisely our bug's mechanism, between
  a module and its loader). Cells (`ContainerRef`), populated once after the
  module mainline finishes; resolved per-frame; with a write companion
  (`unit_scope_lexical_write`) and a writeback-suppression predicate
  (`is_unit_lexical_of`).

Both deliberately stop at the GLOBAL/mainline boundary. Mainline is itself a
compunit whose named subs have exactly the module problem — the "loader" role
is played by any later shadowing block. This ADR extends the unit mechanism
to cover it, rather than inventing a fourth store.

The one real difference from a `unit module`: a module's subs are only called
*after* the module finished loading (snapshot-once is safe), while a mainline
sub is called *while mainline still executes* — its free variables must stay
LIVE (row 1). Hence cells installed **in place** (shared by slot, env, and
store), not a post-hoc snapshot-and-remove.

## Decision

### 1. Store: reuse `unit_lexicals` under a reserved key

Mainline's captured lexicals go into
`unit_lexicals[MAINLINE_UNIT_KEY]` where `MAINLINE_UNIT_KEY` is a reserved
pseudo-unit name no user package can collide with (e.g. `"UNIT«mainline»"`,
constant in `src/runtime/mod.rs` next to the field). Reuse buys, with zero
new plumbing: GC rooting (`src/runtime/gc_roots.rs:199`), thread-clone
propagation (`src/runtime/runtime_thread.rs:543` — cells are shared, so
cross-thread reads stay live, fixing row 7), the read redirect
(`get_env_with_main_alias`, `vm_env_helpers.rs:628`), the write redirect
(`set_env_with_main_alias_sym`, `vm_env_helpers.rs:775`, and the direct
`SetGlobal` site `vm_exec_dispatch.rs:1476`), and the strict-mode
declaredness probe (`has_unit_scope_lexical`, `vm_exec_dispatch.rs:958`).

### 2. Capture: eager, at named-sub registration

In `exec_register_sub_op`, after registration (on **both** the hoisted and
the in-sequence pass, like the `escaped_our` block at
`vm_register_sub_ops.rs:394-411` — the hoisted pass finds nothing, the
in-sequence pass overwrites with the live cells), when ALL of:

- `block_scope_depth() == 0` and not in EVAL (the existing `__mutsu_in_eval`
  check; EVAL'd units also run with raised block depth, so the depth gate
  already excludes them — keep the explicit check anyway),
- `current_package() == "GLOBAL"` (a package-block sub is `package_lexicals`'
  jurisdiction),
- the routine stack is empty (a `sub inner` registered during another sub's
  call is not mainline — see Limitations),
- no module compunit's mainline is currently running (a `use`d module's
  top-level subs register under GLOBAL at depth 0 too; gate on a module-load
  depth counter — reuse one if it exists around `run_modules`' `run_block`
  (`run_modules.rs:801`), else introduce it),
- not a thread clone,

then for each name in the primary compiled body's `free_var_syms`
(`compute_free_vars`, `src/opcode.rs:5054` — computed unconditionally for
named-sub bodies, no new compiler analysis; union over `signature_alternates`
bodies too):

- keep only plain user lexicals (`crate::env::is_plain_user_lexical`) that
  are **scalars** (sigil-less env keys; `@`/`%`/`&` are a follow-up, see
  Limitations),
- require a **mainline local slot** (`code.locals` rposition — `code` here IS
  mainline's CompiledCode and `self.locals` its live slots, since
  `RegisterSub` executes in mainline's frame). No slot → not a mainline
  `my` → skip (constants, undeclared/dynamic names, `our`-only names keep
  legacy behavior),
- skip names that are `our`-linked, `state`-backed, or (under shadow slots)
  duplicated (`dup_named_locals`) — same discipline as
  `box_captured_lexicals`, and reuse its type-constrained-scalar skip
  predicate (`vm_register_ops.rs:824-879`) by extracting a shared helper. A
  skipped name is simply not captured: legacy dynamic behavior, no partial
  state,
- **box in place**: if the slot value is already a `ContainerRef`, reuse the
  cell; else wrap it (`into_container_ref`) and write the cell back to the
  slot AND the env key — exactly the `box_captured_lexicals` install shape
  (`vm_register_ops.rs:759`) — and insert the cell into
  `unit_lexicals[MAINLINE_UNIT_KEY][name]`,
- record the sub's name in a new `mainline_lexical_subs: HashSet<String>` on
  `Interpreter` (cloned into threads alongside `unit_lexicals`).

Boxing in place is what keeps row 1 (live mutation) working for free:
mainline's own later `$client = ...` writes through the cell via the existing
boxed-slot assignment path — the same, already-exercised state that
`box_captured_lexicals` produces when a mainline lexical is captured by an
escaping closure.

### 3. Resolution: a frame predicate, checked only on the last frame

Extend `unit_lexical_slot` (`vm_env_helpers.rs:386-420`) with one candidate,
tried first and gated cheaply:

```
mainline_lexical_frame_active() :=
    !unit_lexicals[MAINLINE_UNIT_KEY].is_empty()      // map presence first
    && last routine frame exists, !is_block
    && frame.package == "GLOBAL"
    && mainline_lexical_subs.contains(frame.name)
```

When true, look the bare name up under `MAINLINE_UNIT_KEY`. Everything
downstream — read deref, write-to-cell redirect skipping env, strict-mode
declaredness — is inherited from the unit machinery unchanged.

**Deliberately last-frame-only, not an innermost-named-frame walk** (the
`escaped_our_sub_names` mechanism walks past block frames; we must not):
row "adv" shows a closure created in the shadow block and invoked from inside
the marked sub must keep reading its own captured `inner`. A block frame on
top therefore opts out of the store, and the closure's captured env answers —
which is correct for every closure origin:

- closure created at mainline: mainline's env entry IS the cell (boxed in
  §2), so it captured the cell — reads stay live;
- closure created in a shadow block: captured the shadow value — raku-correct
  (row adv);
- closure created **inside a marked sub**: handled by §4.

The name-keyed set (rather than a def-level flag threaded through every
call path) means zero changes to the frame-push plumbing
(`accessors_stack.rs:34-99` and its many callers). Accepted imprecision: a
block-declared `my sub` sharing a captured mainline sub's name under package
GLOBAL would match the predicate — same class of imprecision
`escaped_our_sub_names` already accepts, and upgrade path (b) below removes
it if it ever bites.

### 4. Closures created inside a marked sub capture the cells

`capture_closure_env` (`vm_register_ops.rs:500`): when
`mainline_lexical_frame_active()` holds at closure-creation time, override
each captured free variable that has a `MAINLINE_UNIT_KEY` entry with its
cell (both the reflective whole-env path and the slim `free_var_syms` path).
This fixes row 3 — the `.map({ $y })` callback created while `fy()` runs
under a shadow captures `y`'s true cell instead of the caller's shadowed
value — and composes: closures created inside those closures re-capture the
cell from the captured env.

### 5. Writeback suppression

The free-var-write replay (`free_var_writes` →
`pending_rw_writeback_sources`) must not replay a marked sub's write to a
captured mainline lexical into the calling frame's slot — for a call made
inside a shadow block, caller slot and mainline lexical share one
name-addressed slot, and the replay would clobber the shadow (row 2a). The
two suppression sites (`vm_call_named_inner.rs:734-746`,
`vm_call_light_typed.rs:613-618`) currently ask
`is_unit_lexical_of(&cf.package, name)`, which rejects `pkg == "GLOBAL"`.
Add the mainline arm: suppress when the callee is in
`mainline_lexical_subs` and the name is in the mainline map. (The overlay
return-merge needs nothing: writes redirected to the cell never enter the
overlay.)

## Eager vs lazy boxing

Lazy ("box only when a same-named `my` is about to shadow a captured name")
was considered and **rejected**. Shadow introduction is not one chokepoint:
block `my` declarations, for-loop params (ADR-0023's lane), thread-clone
masks (`thread_redeclared_vars`), `EVAL`, map/grep params all create
same-named bindings through different mechanisms. An incomplete detector
turns a missed site into a *flaky, load-order-dependent* wrong answer — the
exact failure class CLAUDE.md's gain/risk definitions rank as the worst
outcome, with a concrete precedent (the by-value capture regression,
`S12-construction/roles-6e.t`). Eager cells are sound by construction: the
cell IS the binding, and every reader/writer that can name it shares it.

This is also consistent with the existing capture philosophy: a registry sub
is the ultimate *escaping* capture (callable from anywhere, any thread), and
escaping captures get cells (`needs_cell_locals`, `box_captured_lexicals`
trigger B).

### Measured cost basis (2026-08-10)

- **Registration**: one-time per mainline sub declaration, O(free vars)
  hash/box ops — noise next to what registration already does (clears eight
  dispatch caches, `vm_register_sub_ops.rs:315-323`).
- **Bench suite survey** (all 23 `benchmarks/*.raku`): 19 have zero mainline
  subs; the 4 that do (`fib`, `bench-fib`, `bench-tak`, `debug-guard`) have
  zero capture candidates after the slot gate — `fib`/`tak` read only
  params, `debug-guard`'s `DEBUG` is a `constant` (no `my` slot). So the
  entire bench suite runs with an **empty map**, and the empty-map gate makes
  every new check one `is_empty` test that `unit_scope_lexical` already
  performs today (`vm_env_helpers.rs:387`).
- **Programs that do capture**: cost is one HashSet name lookup per by-name
  env read while a marked sub's frame is on top — by-name reads are already
  the slow lane (hot paths use slots). Boxed mainline lexicals add cell
  indirection to their own slot reads, identical to what closure capture of
  the same variable already causes.
- **Verification mandated at implementation time**: add `MUTSU_VM_STATS`
  counters (`mainline_lexical_boxes`, `mainline_lexical_hits`); assert
  boxes == 0 across `benchmarks/` (debug build per the counters rule); final
  wall-clock verdict from bench CI history per CLAUDE.md (no local A/B as
  source of truth).

## Alternatives rejected

- **(a) Make named subs real closures** (capture a per-sub env at
  registration, run the body under it — the block-lexical escape hatch shape,
  `vm_register_sub_ops.rs:431-505`, generalized): the architecturally
  "purest" reading of lexical scope, but it changes *everything* a mainline
  sub can see (dynamic vars, `our` aliases, `use`-introduced names all flow
  through the ambient env today), duplicates env snapshots per declaration,
  and turns registry defs — shared across threads — into env carriers. That
  is a Slice-F-endpoint-sized rework, not a bug fix; the authoritative-store
  overlay is the established, incremental pattern (two precedents).
- **(b) Def-level flag threaded to every routine-frame push**: semantically
  identical to §3 with the name-collision imprecision removed, but touches
  every call path that pushes frames (`vm_call_named_inner`,
  `vm_call_light_typed`, fast paths, slow path, Sub-value dispatch) — each
  missed site is a silent resolution hole. Kept as the upgrade path if the
  name-set imprecision ever bites; not worth the plumbing now.
- **(c) Lazy collision-triggered boxing**: rejected above (unsound detector
  ⇒ flaky).
- **(d) Inject cells into the callee overlay at call entry**: reads would
  work, but by-name env writes *replace* entries (no write-through at the
  env layer — that is why `unit_scope_lexical_write` exists), so the write
  side breaks and the overlay return-merge re-clobbers the caller. Rejected.
- **(e) Innermost-named-frame walk for the predicate**: breaks row "adv"
  (raku-verified `inner`); rejected.

## Implementation plan (for the implementing session)

1. **Fields + constant**: `MAINLINE_UNIT_KEY` const;
   `mainline_lexical_subs: HashSet<String>` on `Interpreter`
   (`runtime/mod.rs`), initialized empty (`runtime_init.rs:2223` area),
   cloned in `clone_for_thread` (`runtime_thread.rs:543` area). Module-load
   depth gate if no existing flag serves (`run_modules.rs:801` area).
2. **Capture** in `exec_register_sub_op` (§2), placed with the
   `escaped_our` block so both registration passes run it. Extract the
   type-constrained-skip predicate from `box_captured_lexicals` into a
   shared helper rather than duplicating it.
3. **Predicate + candidate** in `unit_lexical_slot` (§3), as a helper
   (`mainline_lexical_frame_active`) so the write path and capture injection
   share it. Audit that `unit_scope_lexical_write` and
   `has_unit_scope_lexical` behave through the new candidate (they resolve
   via `unit_lexical_slot`, so they should inherit it — verify with row 2
   and a `use strict` probe).
4. **Writeback suppression** at the two `is_unit_lexical_of` call sites
   (§5).
5. **Capture injection** in `capture_closure_env` (§4), both paths.
6. **Counters** (`mainline_lexical_boxes` / `mainline_lexical_hits`) behind
   `MUTSU_VM_STATS`.
7. **Pin test** `t/named-sub-lexical-scope.t` covering the full matrix
   (rows 1-8 + adv, expected = the raku column), plus the two ticket repros
   under `tmp/`. Full `make test`; roast delegated to CI (blast radius is
   every mainline named sub with free variables — do not cherry-pick a local
   subset). Watch bench CI after merge.
8. On completion, `git mv` the deep ticket to `news/2026-08/` per the todo
   lifecycle, and update this ADR's Status.

## Known limitations / follow-ups (all keep today's behavior, none regress)

- **`@`/`%`/`&` free variables**: excluded from slice 1. `@`/`%` are
  Gc-shared so in-place mutation already flows; rebinding/shadowing of them
  through a named sub still has the old dynamic behavior. Cell-ifying them
  intersects the ADR-0010 atomic lanes and Track B and should be its own
  slice.
- **Nested named subs** (`sub outer { sub inner { $x } }`) and **subs
  declared inside blocks** (depth > 0, the `BLOCK_LEXICAL_SUB_PREFIX`
  family): their free vars keep legacy resolution; the same store-plus-
  predicate pattern extends to them later (per-declaration-scope keys
  instead of the single mainline key).
- **Textual-order edge**: a shadowed call *before* the sub's textual
  declaration (`my $c; { my $c; f() }; sub f { $c }`) still resolves
  dynamically — capture has not run yet at that call. Raku-correct programs
  hit this only in contrived orderings; fixing it needs compile-time scope
  analysis, out of scope here.
- **Name-set imprecision** (§3): upgrade path (b) if a real program collides.
- **map/grep callback params** as shadow sources are covered on the *read*
  side by this ADR (the sub consults the store no matter what kind of
  binding shadows the env key); the callback-param capture questions from
  ADR-0023's follow-up list are unaffected.
