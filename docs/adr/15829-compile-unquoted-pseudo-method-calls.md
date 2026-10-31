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

Compile an **unquoted, argument-less** call of `.WHAT`, `.WHO`, `.HOW`, `.WHERE`, `.VAR` and
`.DEFINITE` (the six names `is_mop_pseudo_method` lists, which Rakudo compiles as macros) to a
dedicated opcode `CallPseudoMethod(kind)`; the operand is a small enum, so `size_of::<OpCode>()`
stays within 48 bytes. `.WHY` and `.WHICH` are not in the set: they are ordinary overridable
methods. A *quoted* call (`$x."WHAT"()`) is lowered to the generic method-call opcode at compile
time, so the opcode never sees one and needs no quoted flag.

**`.^name` is not a pseudo-method and is not a kind of this opcode.** It is `$x.HOW.name($x)`, an
ordinary metamethod call that a type can override (`class A { method ^name(\o) { "Custom" } }`
makes `A.^name` and `A.new.^name` answer `Custom`) and that a custom metaclass changes (AttrX::Mooish
itself uses `CustomClassHOW is Metamodel::ClassHOW`). This ADR takes the lowering option: the
compiler rewrites `.^name` (and the other `.^` metamethods, later) to `.HOW.name($obj)` with `.HOW`
as the opcode below and an ordinary generic call after it. Semantics hold by construction: `.HOW`
returns the receiver's real HOW object (stock, custom, or the one a `method ^name` was added to),
and the following call resolves on it as today. The first slice only introduces the `.HOW` opcode
and the lowering for `^name`; the lowering is justified by the parity test, whose oracle for
`^name` is `raku` (the reviewer's `method ^name` example and a `Metamodel::ClassHOW` subclass).

Execution is **guard + existing path as the miss tail** (the ADR-15737 D1 shape; no new slow path):

1. Evaluate the receiver exactly as the call does today. The new opcode replaces **both**
   lowerings of an unquoted, argument-less pseudo-method call: `CallMethod` and `CallMethodMut`
   (a call on a variable receiver is emitted as `CallMethodMut`, and the expensive chain in
   Context starts at `exec_call_method_mut_op`; an opcode that replaced only `CallMethod` would
   rarely be reached). A pseudo-method needs no write-back (it never rebinds its receiver), so the
   mut form lowers to the same arm; the receiver is read from its slot as the mut form does today.
   Both opcodes already carry the `quoted` flag, which is what makes the compile-time split of
   quoted calls straightforward.
2. If the receiver is a *plain value or type object* (Int, Num, Str, Bool, a built-in or user type
   object) and the kind's per-kind guard holds, call the same runtime function the generic arm
   calls today directly, with no method resolution: `dispatch_what`, `dispatch_how`,
   `dispatch_who` (`methods_introspect.rs`); `.DEFINITE` is the `Mu.DEFINITE` row's `definite`
   (`builtins/method_table/ctors_mop/mu_identity.rs`), and `.WHERE` is that file's `where_in`
   (`Mu.WHERE`: the object's identity address, with the NativeCall-handle exception
   `try_native_handle_repr_where`, so a receiver that is a NativeCall handle takes the generic
   path). `.WHERE` and `.DEFINITE` are the two kinds with no `dispatch_*` function; the parity
   test pins them against `raku` before they join the first slice.
3. Otherwise run the existing generic method-call opcode path unchanged.

The guards are the decision's substance, because each marks a place where the generic path does
something the fast arm does not:

- **Proxy, `VarRef` and container receivers** (`container_type_metadata`, `auto_fetch_proxy`):
  always the generic path.
- **`.VAR` is not in the first slice.** `$x.VAR` on a variable answers a `Scalar` (`raku -e 'my $i
  = 1; say $i.VAR.WHAT'` gives `(Scalar)`), not the target, and has descriptor rules (ADR-0064's
  `var_meta_descriptor_defers` / `try_var_meta_delegate`, ADR-0057's cell boxing). The earlier
  draft's "take only the case where the generic arm returns the target itself" does not cover the
  case that was measured (`$x.VAR.defined.Int`, 55.6k Ir). `.VAR` joins once those rules are
  covered by the parity test.
- **A `Failure` receiver** takes the generic path on day one, but not because it must explode: these
  are macros, so a Failure is not "used" (`raku -e 'my $f = Failure.new("x"); say $f.WHAT;
  say $f.DEFINITE'` prints `(Failure)` and `True`). It is simply not covered by the parity test yet;
  once `Failure.new("x").WHAT/.DEFINITE/.HOW` are pinned against `raku` it can be admitted on the
  fast arm.
- **`augment` / `.wrap` of a built-in type** cannot override an unquoted pseudo-method, so no guard
  is needed for them; this is pinned by tests (below), not assumed.

## Expected effect

**Not yet established.** The earlier draft's "up to about -10% of the Mooish iteration" is
withdrawn: it counted ten pseudo-method calls per iteration without knowing their receivers. What is
measured so far:

- A pseudo-method call on a plain Int costs 26-36k Ir on main (`$x.WHAT.defined.Int` 36.0k,
  `$x.HOW.defined.Int` 69.4k); the target for the plain-value arm is <= 8k Ir per call.
- `MUTSU_VM_STATS=1` counts 7 `.VAR` interpreter fallbacks per Mooish iteration, but `.VAR` is out
  of the first slice, and a count of the pseudo-method calls that reach
  `call_method_with_values_inner` on the same loop found 1 per iteration, a `.VAR` on a `Proxy`
  (which the Proxy guard sends to the generic path). The other fallbacks are counted on other
  entry points (`call_method_mut_with_values`, the mut opcodes) and their receivers were not
  recorded.
- Many Mooish `.VAR`/`.WHAT`/`.HOW` calls probably come from `nqp::what_nd` / `nqp::how_nd`, which
  the compiler lowers to `.VAR.WHAT` / `.VAR.HOW` (`src/compiler/nqp_object_forms.rs:35-39`). If
  their operands are attribute containers or Proxies, the guards send them to the generic path and
  the first slice would not move Mooish at all.

**Precondition for accepting this ADR's estimate:** a table of the pseudo-method calls in one
Mooish iteration (and in Red's `t/38-equal` loop once it can be run), by kind and receiver class
(plain value, type object, Instance, container, Proxy, Failure), recorded with a stats hook at the
call-site level so every entry point is counted. The same table records the cost of the `^name`
rewrite: measure `.HOW.name($x)` (the opcode plus the remaining generic `.name($obj)` call on the
HOW object) against today's `.^name` (26.4k Ir) before committing to the lowering. If the rewritten
form is not cheaper, the rewrite buys only semantic uniformity for the `.^` metamethods, and the
ADR keeps it for that reason alone or drops it. It decides the first slice: if most land in
container/Proxy/Instance, the realistic first slice is `.WHAT`/`.HOW` (and the `^name` rewrite)
for values and type objects, which helps `Test`-style code and roast helpers, not Mooish. The ADR
does **not** claim to close #15688 or #15498 either way.

## Consequences

- Positive: removes the most expensive recurring calls on plain values and type objects (26-70k Ir
  each on main) without adding a cache; whether that reaches the measured workloads (Mooish, Red)
  is the open question under Expected effect; the miss tail is today's code, so a wrong guard degrades to today's speed, not to a wrong
  answer; the deleted work (the per-call user-override probes for these six names) is gone from the
  hot path.
- Negative: a second implementation site for the six names must stay in lock-step with the generic
  arms. Mitigation: the opcode calls the same runtime functions and a parity test runs every kind
  against the generic path (a quoted `$x."WHAT"()` is the oracle for the unquoted result on plain
  receivers; `raku` is the oracle for everything else, and the only oracle for `^name`, which a
  quoted call cannot be).
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
  (D1a, the Raku-level call floor); this ADR covers only the six pseudo-methods and the `^name` rewrite.

## Open questions

- Which receivers does the guard admit on day one? Proposal: Int/Num/Str/Bool values and any type
  object; Instances only after the parity test covers `handles`, `FALLBACK` and role-mixed
  receivers; Failure once pinned against `raku`.
- Is the per-iteration table (Expected effect) enough to decide the first slice, or should `.VAR`
  be measured separately on attribute containers first?
