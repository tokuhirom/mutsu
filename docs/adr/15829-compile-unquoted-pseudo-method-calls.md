# ADR-15829: Compile unquoted MOP pseudo-method calls to a guarded opcode

- **Status**: Proposed
- **Issue**: [#15829](https://github.com/tokuhirom/mutsu/issues/15829) (child of
  [#15737](https://github.com/tokuhirom/mutsu/issues/15737); motivated by
  [#15688](https://github.com/tokuhirom/mutsu/issues/15688))
- **Input**: ADR-15737 and its slices (#15809, #15813, #15821), ADR-11276 (the built-in method
  lane), `src/runtime/builtin_shadow.rs`, the perf-tuning skill (section 5a)

## Context

ADR-15737 expected the Raku-level call floor to be the main lever on the AttrX::Mooish iteration
(#15688). Measurement after its first slices says otherwise (profiling build, `Lazy.new; $i.foo`):

- One iteration runs about 42 method-call and 24 function-call opcodes and pushes about 16
  Raku-level frames (`MUTSU_VM_STATS=1`, a gdb hit count on `pop_call_frame`); the ADR had assumed
  "a few hundred calls". The call floor is therefore about a quarter of the 4M-Ir iteration.
- 21 of the 42 method-call opcodes per iteration are interpreter fallbacks (`call_method_with_values`).
  The most frequent are the MOP pseudo-methods and `.^` metamethods: `.VAR` seven times per
  iteration, plus `.HOW`/`.WHAT`/`.^name`-class calls.
- Those calls are far above the builtin lane. Slope between 100 and 300 iterations of
  `$s += EXPR` on an Int (main@259e9113): `$x.chars` 2.6k Ir, `$x.defined.Int` 6.8k,
  `$x.WHAT.defined.Int` 36.0k, `$x.^name.chars` 26.4k, `$x.VAR.defined.Int` 55.6k,
  `$x.HOW.defined.Int` 69.4k.
- The cost is the chain, not one function: `exec_call_method_mut_op` ->
  `call_method_with_values_inner` -> `try_native_method` -> `dispatch_method_by_name_1` -> the
  `methods_dispatch_match.rs` arms (`"WHAT" if args.is_empty() && !quoted_pseudo`, `"VAR"`, `"HOW"`),
  about 25 functions of at most 1.7k Ir each. #15821 removed the avoidable name scans
  (-25% for `.WHAT`) and a further micro-slice (one `as_str` read, branch
  `perf/15737-meta-call-path`) measured -4% and was not pursued: per section 5a, shaving the
  chain does not change its structure.

Rakudo treats `WHAT`, `WHO`, `HOW`, `WHERE`, `VAR` and `DEFINITE` as macros: a user method of that
name is reachable only through the quoted form `$x."WHAT"()`. mutsu already encodes this
(`is_mop_pseudo_method`, `user_shadows_builtin(..., quoted)`), so for the *unquoted* form the
resolution work (user-override probes, `native_lever_a_user_override`, MRO reads) is, for a plain
receiver, known to have one answer.

## Decision (proposed)

Compile an **unquoted, argument-less** call of `.WHAT`, `.WHO`, `.HOW`, `.WHERE`, `.VAR`,
`.DEFINITE` and the `^name` metamethod to a dedicated opcode `CallPseudoMethod(kind)` (the operand
is a small enum, so `size_of::<OpCode>()` stays within 48 bytes). `.WHY` is not in the set: it is an
ordinary overridable method.

Execution is **guard + existing path as the miss tail** (the ADR-15737 D1 shape; no new slow path):

1. Evaluate the receiver exactly as `CallMethod` does today.
2. If the receiver is a *plain value or type object* (Int, Num, Str, Bool, a built-in or user type
   object, a non-Proxy non-`VarRef` non-container value) and the kind's per-kind guard holds, call the
   same runtime function the generic arm calls today (`dispatch_what`, `dispatch_how`,
   `dispatch_who`, the `.VAR` identity, `dispatch_caret_name`) directly, with no method resolution.
3. Otherwise run the existing generic method-call opcode path unchanged.

The guards are the decision's substance, because each marks a place where the generic path does
something the fast arm does not:

- **Proxy, `VarRef` and container receivers** (`container_type_metadata`, `auto_fetch_proxy`):
  always the generic path.
- **`.VAR`** has descriptor rules (ADR-0064, `var_meta_descriptor_defers`,
  `try_var_meta_delegate`): the opcode takes only the case where the generic arm returns the target
  itself.
- **A `Failure` receiver** must still explode on use; generic path.
- **`augment` / `.wrap` of a built-in type** cannot override an unquoted pseudo-method, so no guard
  is needed for them; this is pinned by tests (below), not assumed.

## Expected effect

Per-call cost for a plain receiver falls from 26-70k Ir to a few k (the opcode, the runtime function,
the result), a target of <= 8k Ir per call (the issue's close condition). For Mooish that is
about ten calls per iteration, so up to about -10% of its 4M Ir; the same calls recur in Red and in
roast helpers. It does **not** close #15688 (5x of rakudo) or #15498 on its own.

## Consequences

- Positive: removes the most expensive recurring calls in the measured workloads without adding a
  cache; the miss tail is today's code, so a wrong guard degrades to today's speed, not to a wrong
  answer; the deleted work (the per-call user-override probes for these six names) is gone from the
  hot path.
- Negative: a second implementation site for the six names must stay in lock-step with the generic
  arms. Mitigation: the opcode calls the same runtime functions and a parity test runs every kind
  against the generic path (`$x."WHAT"()` quoted is the oracle for the unquoted result on plain
  receivers; `raku` is the oracle for the rest).
- Negative: touches the compiler's method-call lowering and one more `OpCode` arm in
  `exec_one_dispatch` with its `// Cost:` line.

## Alternatives considered

- **Add `WHAT`/`HOW`/`VAR` as rows of the built-in method table** (ADR-11276): `receiver_step`
  already special-cases them (`"VAR" | "WHAT" | "WHICH" | ... => observes_container`), and the rows
  need an interpreter handler plus the same container guards, so the table gets the complexity
  without removing the resolution chain in front of it. Rejected as the larger change for the same
  guards.
- **Keep trimming the chain** (the as_str hoist, name memos): done where measurable (#15821); the
  remainder is 1-4% per step.
- **Fix the interpreter-fallback count generally** (the 21 fallbacks per iteration, which also include
  user methods of the Proxy classes such as `get_value`/`store-value`): a different problem
  (D1a, the Raku-level call floor); this ADR covers only the six pseudo-methods and `^name`.

## Open questions

- Is `^name` best a kind of this opcode, or a rewrite to `.HOW.name($obj)` with `.HOW` as the opcode?
  The measured cost (26k Ir) says the former is simpler; the latter composes with a future `.^`
  family.
- Which receivers does the guard admit on day one? Proposal: Int/Num/Str/Bool values and any
  type object; Instances only after the parity test covers `handles`, `FALLBACK` and role-mixed
  receivers.
- Does the opcode need the quoted-call flag (`quoted_pseudo`) in its operand, or is "quoted" lowered
  to the generic opcode at compile time (proposal: the latter, so the opcode never sees a quoted
  call)?
