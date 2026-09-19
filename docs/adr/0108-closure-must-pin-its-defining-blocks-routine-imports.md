# ADR-0108: A closure must pin the routine imports live in its defining block

- **Status**: Proposed
- **Date**: 2026-09-19
- **Addresses**: [GitHub issue #8751](https://github.com/tokuhirom/mutsu/issues/8751)
- **Related**: [ADR-0081](0081-compunit-scoped-module-import-aliases.md) (a
  sibling manifestation of the same "no real lexical scope, just a dynamic
  flat-registry stack" gap, for a *variable/type* import leaking to the wrong
  side rather than disappearing from the right one),
  [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md) (the
  env-tier mechanism that already gives *variable* capture the real lexical
  lifetime this ADR wants for routine calls),
  [ADR-0086](0086-builtin-dynamics-are-not-closure-capture-material.md) (the
  per-interpreter base tier `Env` already consults below the chain — the same
  shape §3.2 below proposes reusing)
- Refs: [#7612](https://github.com/tokuhirom/mutsu/issues/7612), the export
  wrapper fix (`news/2026-09/export-sub-wrapper-same-name-shadow.md`, PR
  closing [#8746](https://github.com/tokuhirom/mutsu/issues/8746)) whose
  regression test surfaced this bug

## 1. Context

### 1.1 The observable bug

```raku
# Inner.rakumod
unit module Inner;
sub real-greet(:$from = "unset") is export {
    "hello from $from";
}

# Outer.rakumod
sub EXPORT(--> Map()) {
    use Inner;
    '&wrapped-greet' => -> |c {
        real-greet(|c, :from<wrapped>);
    },
}
unit module Outer;
```

```
$ raku -I . r.raku
hello from wrapped
$ mutsu -I . r.raku
Unknown call: real-greet
  in block <unit> at Outer.rakumod line 3
  in block <unit> at r.raku line 3
```

The closure literal is created *inside* `sub EXPORT`'s own body, which does
`use Inner;` internally. `sub EXPORT` returns immediately, and the closure is
invoked much later — well after `sub EXPORT`'s own call frame, and the import
scope it opened, have both gone away.

### 1.2 The root cause, traced directly (not inferred)

The issue's own hypothesis blamed a registration asymmetry between `unit
module Inner` and a bare-file `Inner` (no `unit module` line): it guessed a
`unit module`'s exported sub registers only under the ambiguous `GLOBAL::name`
key, indistinguishable from an import alias, and is stripped for that reason.
That hypothesis does not survive tracing the actual registry keys through
`pop_import_scope` (instrumented with a temporary `eprintln!`, then reverted):

```
POP key=Inner::real-greet              keep=true  (module's own def; package-qualified, survives unconditionally)
POP key=Inner::EXPORT::DEFAULT::real-greet keep=true  (survives)
POP key=Inner::EXPORT::ALL::real-greet     keep=true  (survives)
POP key=EXPORT::DEFAULT::real-greet        keep=true  (survives — in module_registered_functions)
POP key=EXPORT::ALL::real-greet            keep=true  (survives — in module_registered_functions)
POP key=GLOBAL::real-greet                 keep=false (STRIPPED)
```

`Inner::real-greet` — the module's own true definition, package-qualified —
survives the pop exactly as `pop_import_scope`'s design intends. The **only**
key removed is `GLOBAL::real-greet`, and that key is not the module's
definition at all: it is the alias `import_module` installed for *this
specific* `use Inner;`, because `sub EXPORT`'s own body runs at
`current_package() == GLOBAL`. Removing an import-scope's own aliases when its
block exits is not a bug — it is the exact mechanism `roast/S11-modules/
lexical.t` requires (`{ use Foo } foo()` must die). The bug is that the
closure, invoked long after that pop, has no other route to `real-greet`:
bareword call resolution (`find_compiled_function_inner`,
`src/vm/vm_call_resolve.rs`) walks the caller's own enclosing packages and then
`GLOBAL::name` — it does not, and must not, search every loaded module's
package for a same-named sub (that would make any module's internal routine
leak into any importer, the opposite of what `S11-modules/lexical.t` pins).

So the general shape is exactly what the issue named: **mutsu's routine
registry is a dynamic (push/pop) structure, and a closure that outlives the
dynamic extent of the block that imported into it loses the import — even
though the import's underlying definition is still sitting in the registry
under its permanent, package-qualified key.** This is a different symptom of
the same "no real lexical capture for anything outside `env`" gap ADR-0081
names for variables/types leaking the *other* direction.

### 1.3 The bare-file "workaround" is not a fix

The issue notes that writing `Inner` as a bare-file module (no `unit module`
line) makes the repro "work". Tracing that variant shows why, and that it is
not a fix for the general class:

```
POP key=GLOBAL::real-greet   keep=true  in_module_keys=true  (already true BEFORE this push)
```

A bare-file module has no package, so its own top-level `is export sub`
registers *directly* as `GLOBAL::real-greet` at module-load time — before any
`use` statement's aliasing step runs — and that load-time registration is
recorded into `module_registered_functions`, the permanent "this is a module's
own definition, never strip it" set. The alias `import_module` installs for
`sub EXPORT`'s `use Inner;` happens to collide with that same literal key, so
it is never actually treated as an ephemeral, scope-bound alias to begin with.
This is accidental: a bare, non-closure statement placed *after* the
importing block, with no closure involved at all, would incorrectly keep
seeing `real-greet` too — a distinct, over-permissive masking bug, not
evidence that the closure-capture problem is solved. Do not generalize from
this shape; it is a coincidence of bare-file modules registering at `GLOBAL`
by construction, not a working reference implementation.

### 1.4 Why this needs a design, not a registry-key patch

Three fixes were considered and rejected before writing this ADR — see §4.
None survives contact with `roast/S11-modules/lexical.t`, which pins that a
call reached *after* the block, with no closure, must still fail. Any fix
must therefore attach the survival to the closure's own identity, not to a
registry key's shape or a name's provenance.

## 2. Decision (proposed)

Give a closure a private, immutable route to the routine names its defining
block imported, captured at the moment the closure is created — the same
lexical-lifetime guarantee ADR-0092 already gives captured *variables* via the
env-tier chain, extended to cover the routine registry.

### 2.1 What must be captured, and when

At closure creation (`exec_make_block_closure_op`,
`src/vm/vm_register_sub_ops.rs`), if `self.import_scope_stack` is non-empty
(there is at least one currently-open `use`-containing block on the dynamic
stack), snapshot the routine aliases that block's own `use` installed and are
not yet in the registry's permanent (package-qualified / `module_registered_
functions`) survivor set — i.e. exactly the set `pop_import_scope` would
strip if the innermost open scope closed right now. Do this against the
*outermost* open scope's pre-push snapshot, so a closure nested under several
stacked `use`-containing blocks captures the union, not just the innermost
delta. Store the result as `Option<Arc<HashMap<Symbol, Arc<FunctionDef>>>>` on
`SubData`, parallel to (not merged with) the existing env capture — `None`
when no import scope is open at creation time, which is the overwhelmingly
common case and costs nothing beyond the `is_empty()` check.

### 2.2 How the capture is consulted at call time

A bareword call that would otherwise fail with "Unknown call" (the tail of
`exec_call_sanitized`, `src/runtime/calls.rs`) must first check whether the
*currently executing* routine/closure carries this capture, and resolve
against it as a last resort before raising the error. This requires knowing,
at that point, which `SubData` (if any) is executing — threaded via an
RAII-guarded stack on `Interpreter` (push on entry to a compiled closure body
that carries a non-`None` capture, pop in the guard's `Drop` so an error
unwinding through `?` still pops correctly), not by re-deriving it from
`RoutineFrame` (which carries only interned name/package symbols today, not a
`SubData` reference).

A nested closure created *while* an outer captured closure is executing (but
after the original import scope has long since popped) must still see the
capture: closure creation should union `self.import_scope_stack`'s live
aliases (§2.1, ordinarily empty by then) with whatever is on top of this new
active-capture stack, so the inner closure inherits the outer's pinned routine
aliases the same way it already inherits the outer's captured variables
through the env chain.

### 2.2.1 Alternative consulting mechanism — ride the env tier instead

A closure's env capture already has the correct lexical lifetime (ADR-0092);
consider making the *registry* alias also visible through `env` rather than
building a second, parallel capture-and-lookup mechanism:

`import_module` would additionally write `&real-greet` into `env` as a
callable `Value`, using the already-shipped "env callable override" bareword
check built for the `sub EXPORT` wrapper fix
(`export_amp_override_names`/`env_callable_is_lexical_override`,
`src/vm/vm_call_func_ops.rs`) to let a bareword call notice it. `pop_import_
scope` already removes any `env` key it recorded via `record_import_env_key`
on scope exit, so a plain post-block call keeps failing exactly as it does
today. A closure created before the pop already has its own env captured
through the tier chain (§2.1's ADR-0092 machinery), so `&real-greet` survives
in *its* capture even after the live `env`'s copy is removed on pop — with no
new capture field on `SubData` and no new active-capture stack.

This reuses more already-accepted, already-perf-tuned machinery than §2.1/2.2,
but writing an env entry for every routine `use` import changes a hot path
(every bareword call now has one more override candidate to rule out, not just
for the closure case) and needs its own review of `amp_param_shadowed_names`/
`env_callable_is_lexical_override`/`free_var_syms` interactions, since those
were built for the narrower "an EXPORT-installed override" case, not "every
plain routine a `use` ever imports". Whether this alternative or §2.1/2.2's
dedicated capture is the better implementation is an open question for
whoever implements this ADR to settle with a measurement, not a call this ADR
makes; §2.1/2.2 is the default because it changes nothing on the path that
does not go through an open import scope.

## 3. Invariants

- A call reached *after* a `use`-containing block, from ordinary code with no
  closure between the block and the call, keeps failing
  (`roast/S11-modules/lexical.t`'s `{ use Foo } foo()`).
- A closure created inside a `use`-containing block, called after that
  block's own dynamic extent has ended, resolves the names live at its own
  creation point — matching `raku`'s output, not mutsu's current one.
- A closure created *outside* any open import scope pays no capture cost at
  all (`None`, no allocation, one `is_empty()` check).
- A nested closure created during the outer closure's own (deferred)
  invocation still sees the outer's pinned routine names.
- `t/block-use-keeps-nested-module-imports.t` and
  `t/module-reuse-class-in-block.t` (the #7580 fix) keep passing unmodified —
  this ADR's mechanism is additive to, not a replacement for, `pop_import_
  scope`'s existing retention rules.
- A module's own package-qualified definition (`Inner::real-greet`) is never
  touched by this change; the gap is purely in how a closure reaches it once
  its own importing alias is gone.

## 4. Alternatives rejected

### 4.1 Fix the "unit module vs bare-file" registration asymmetry

Rejected: there is no asymmetry to fix. §1.2 traces `Inner::real-greet` (unit
module) surviving the pop exactly like a bare-file module's own definition
would. The removed key (`GLOBAL::real-greet`) is a *this-import's* alias in
both shapes; the bare-file case merely (and accidentally, per §1.3) reuses a
key that is *also* the module's permanent definition, so it never gets treated
as ephemeral to begin with. Re-keying unit-module registration would not
change what the closure needs.

### 4.2 Let bareword resolution fall back to searching loaded modules' packages

Rejected: this is the literal thing `roast/S11-modules/lexical.t` forbids. A
bareword call must not resolve against a module's internal routines merely
because that module happens to be loaded somewhere in the process — that is
dynamic scoping, and Raku's imports are lexical.

### 4.3 Never remove a routine's import alias at block-scope exit

Rejected: this is the pre-#7580 behavior, and reintroducing it fails
`roast/S11-modules/lexical.t` directly. The alias's dynamic-scope removal is
correct for the overwhelming majority of code (a plain call inside the block);
only a closure that captures the name and outlives the block needs a second,
private route to it.

### 4.4 Re-key by source-file / compunit identity (ADR-0081-style)

Considered and set aside for this ADR's scope, not rejected outright: ADR-0081
already proposes package-owned scope tables (`module_scope_lexicals`,
`package_type_aliases`) keyed by the *unit module's* own package identity, for
the opposite-direction bug (an import leaking to the *outer* using scope). A
future slice could plausibly unify that machinery with this ADR's capture, but
ADR-0081's tables are keyed by the module doing the importing's package name,
which is stable across calls — the problem here is specifically about an
*anonymous closure's own, per-instance* creation site, which has no package
identity to key by. The two remain separate decisions until someone has a
concrete design for unifying them.

## 5. Acceptance

Add a focused regression under `t/modules/` (or the closest matching category
per `docs/t-directory-layout.md`) covering, checked against both `raku` and
mutsu:

1. The issue's own repro (`unit module Inner`, closure inside `sub EXPORT`'s
   own `use`-containing body, invoked after `sub EXPORT` returns).
2. The same shape with a bare-file `Inner` (no `unit module` line), to pin the
   *closure* path working for the right reason rather than exercising only
   §1.3's accidental survival.
3. A closure created inside an ordinary `{ use Foo; -> { foo() } }` block (no
   `sub EXPORT` involved), invoked after the block, to isolate the mechanism
   from the export-wrapper machinery entirely.
4. A closure created inside a closure created inside a `use`-containing block
   (nested creation, §2.2's inheritance requirement).
5. `roast/S11-modules/lexical.t` and `roast/S11-modules/require.t` test 10
   (both cited by the existing `pop_import_scope` comments as pinning the
   opposite behavior) must keep passing.
6. `t/block-use-keeps-nested-module-imports.t` and
   `t/module-reuse-class-in-block.t` must keep passing unmodified.

The implementation PR must run the full `make test` + `make roast` gates
(this touches closure creation and call resolution, both extremely hot paths)
and, if §2.2.1's alternative is chosen instead of §2.1/2.2's dedicated
capture, include a callgrind measurement on a closure-heavy benchmark showing
the bareword-call fast path is not regressed for code with no open import
scope.
