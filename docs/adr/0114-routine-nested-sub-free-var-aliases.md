# ADR-0114: A sub declared in a routine reads its free variables through per-activation aliases

- **Status**: Accepted (2026-09-23); implemented in the same PR as the decision.
- **Extends**: [ADR-0024](0024-mainline-lexicals-for-named-subs.md), which gave lexical
  free-variable resolution to mainline and bare-block subs and left routine-nested subs on
  dynamic resolution.
- **Context**: [#9111](https://github.com/tokuhirom/mutsu/issues/9111) (follow-up to #9106 /
  PR #9109). Related: [ADR-0113](0113-frame-lexical-inner-subs.md) (frame-lexical inner
  subs), [#9103](https://github.com/tokuhirom/mutsu/issues/9103).

## 1. Problem

A named sub reads a free variable with `GetGlobal`, by name, in the env live at the call:
the callee runs in an overlay over its caller's env. ADR-0024 redirects those reads to
cells captured at declaration time, but only for a sub declared at mainline or in a bare
block. A `my sub` declared inside a routine still read whatever its caller had under the
name:

```raku
sub sh($p) { my sub t() { $p }; -> { my $p = 99; t() } }
say sh(5)();    # raku: 5    mutsu: (Any)

sub nb($p) { my sub t() { $p }; { my $p = 9; t() } }
say nb(4);      # raku: 4    mutsu: 9
```

A nested block, a `for`/`map` parameter or a closure local with the name of a free
variable of the inner sub all shadowed it. A write had the matching bug: the free-variable
write replay (`free_var_writes`) pushed the value into the caller's same-named slot.

ADR-0024's store cannot serve these subs. Its buckets are keyed by the sub's name, and a
routine-nested sub needs one binding **per activation of the routine** — `sh(1)` and
`sh(2)` may both be alive through closures, and a recursive routine has several activations
on the stack at once.

## 2. Decision

**The declaring frame owns one hidden local per free variable of the inner sub, and each
activation binds it to that variable's cell.** The inner sub's by-name accesses consult the
alias before the ambient env.

- **Compile time** (`compiler/lexsub_aliases.rs`). For a sub declared in a routine body
  (the same gate as `record_lexical_sub_free_vars`: not `multi`, not `our`, not a computed
  name), every free variable (read or written) that is a plain `my` lexical visible from the
  declaration gets a local named `__mutsu_lexsub_<fingerprint>_<serial>_<sub>_<var>` in the
  declaring frame. The plan records `(var, var_slot, alias, alias_slot)` in
  `CompiledSubDeclPlan::lexsub_free_aliases`. The alias names join the sub's entry in
  `Compiler::lexical_sub_free_vars`, so every closure that calls the sub captures them the
  way PR #9109 made it capture the free variables themselves.
- **Binding** (`vm/vm_lexsub_aliases.rs`, from `exec_register_sub_op`, on every execution,
  hoisted and in sequence, and before the ADR-0113 frame-lexical early return, which derives
  the definition only once). A variable that is a local of the declaring frame is boxed in
  place into a `ContainerRef` cell, the same install shape as ADR-0024's capture; a variable
  of an enclosing frame is aliased only when the binding that reaches this frame is already a
  cell. The cell is written to the alias slot and env key, and the sub's name is mapped to
  `(var, alias)` in `Interpreter::lexsub_free_aliases`.
- **Resolution.** While the last routine frame is not a block frame and its name is a key of
  that table, `unit_lexical_slot` / `unit_lexical_slot_mut` / `unit_lexical_container_cell`
  answer a free variable with the alias found in env. The alias is always reachable there:
  the sub runs in an overlay of its caller's env, and a lexical caller is either the
  declaring frame or code that captured the alias. The `GetGlobal` scalar shortcut is gated
  off while such a frame is on top, exactly as it is for ADR-0024.
- **Per activation by construction.** The alias is a local of the declaring body, and
  `CompiledFunction::compute_declared_locals` counts it as callee-local, so the call-return
  merge never copies one activation's alias into its caller.
- **Writes.** The replay of a `free_var_writes` entry into the caller's slot is suppressed
  when the write went through an alias (`is_lexsub_alias_write`, consulted on the named,
  typed-light, positional-light and fast paths).
- **Closures created inside the sub** capture its cells, not the caller's shadows
  (ADR-0024 §4, `inject_lexsub_alias_captures`).
- **TRIR.** `trir_outer_binding` asks the alias table by callee name, and a routine-nested
  sub's resolved bindings are never memoized, since they change per activation.

## 3. Rejected alternatives

- **Per-activation buckets in `unit_lexicals`.** A bucket keyed by an activation id would
  need removal when the last closure holding the activation dies; nothing observes that, so
  every call of a hot routine would leak a bucket.
- **A frame-lexical code object per activation** (the #9103 direction: create a closure-like
  value when the declaration runs, and call through it). It gives the same semantics but
  touches every call path of four call opcodes and the JIT, and ADR-0113 only just made
  those call sites compile-time resolved without a value. The alias keeps the call path
  untouched; a later code-object slice can bind the same aliases.
- **Rewriting the inner sub's variable names at compile time.** Every opcode that names a
  variable would need rewriting, including those of closures created inside the sub, and a
  name the alias does not cover (a call before the declaration ran) would have no fallback.
- **Aliasing only when a shadow is detected at compile time.** Cheaper, but the detector
  would have to see every way a scope can introduce a same-named binding (blocks, loop and
  callback parameters, closures, sibling subs), and a missed shape silently keeps the old
  answer. The alias costs one cell per free variable per activation.

## 4. Consequences and limits

- A routine that declares such a sub pays, per call, one cell per free variable and an env
  insert per alias. The inner sub's by-name reads pay one table probe and one env probe.
- **Name-keyed table.** As in ADR-0024 §3, the table is keyed by the sub's name. A different
  routine of the same name running while another's alias is visible would read it; the
  alias names are unique per declaration, and the alias must also be live in the env chain,
  which narrows this to a same-named sub called from inside the declaring routine.
- **Unchanged:** `state` and `our` variables (their storage lives outside the slot), `multi`
  and `our` inner subs, a call made before the declaration ran for a variable that was not
  yet initialized, and `&`-sigiled free variables keep the dynamic resolution.
