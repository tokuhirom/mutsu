# ADR-11834: Integer atomics classify their target through the lexical scope chain

- **Status**: Accepted (2026-10-06)
- **Date**: 2026-10-06
- **Issue**: [#11834](https://github.com/tokuhirom/mutsu/issues/11834)
- **Related**: [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md)
  (a constraint belongs to the container), [ADR-0062](0062-atomic-lane-anchors-to-the-published-value.md)
  (the name-keyed atomic lane), [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md)
  (slot-addressed binding metadata), [ADR-11553](11553-nqp-representation-and-reference-identity.md)
  (native kind on a reference), [#12007](https://github.com/tokuhirom/mutsu/issues/12007)
  (the cell half, still open)

## 1. Problem

`$x⚛++`, `$x ⚛+= n`, `atomic-fetch-add($x, n)`, `nqp::atomicinc_i($x)` and the rest of the
integer atomics took **any** scalar, attribute or array/hash element. Rakudo gives them one
candidate, `atomicint $target is rw`, so the target has to be a native-integer container -- an
`int` / `atomicint` variable or attribute, or an element of a native-int array. On a plain
`my $x` it is a routine dispatch failure (`X::Multi::NoMatch`, "Cannot resolve caller
postfix:<⚛++>(Int:D); the following candidates match the type but require mutable arguments"),
and at the `nqp::` level MoarVM's own `X::AdHoc` ("Can only do integer atomic operations on a
container referencing a native integer"). `atomic-fetch`, `atomic-assign`, `cas` and `⚛=` have
`$target is rw` candidates and stay legal on any scalar.

A first attempt added a run-time check that read the target's type from the frame's name-keyed
metadata (`__mutsu_type::$x`) and the cell constraint. It matched Rakudo on every case in the
issue and was dropped: it also refused a **real** `atomicint`. A module-scope `my atomicint
$hits` reached from an exported routine running on a worker thread has no `__mutsu_type::$hits`
in that thread's frame, and its value lives in the name-keyed lane, not in a typed cell
(`t/concurrency/thread-lock/module-scope-atomicint-from-imported-routine.t`). The mutsu
container does not carry its native kind (the root cause #11553 describes for `nqp::iscont_i`),
so "no type found" cannot mean "not native". A false refusal of a correct program is much worse
than the leniency the check removes.

## 2. Decision

**Classify the target where the information is reliable, and refuse only a target positively
known not to be native.**

1. **Lexical declarations are classified by the compiler.** Whether `$x` is a native-integer
   container is settled by its declaration, which is visible in the scope chain -- Rakudo itself
   decides it lexically. A `ScopeFrame` (`compiler/lex_scope.rs`) now records, next to the slot
   map, the declared type of each plain `my`/`state`/`our` scalar the scope declares (and of
   each parameter that binds a value). The frame is the unit that is popped, parked and handed
   to a nested sub or closure (`inherit_enclosing_scopes`), so a type cannot outlive or be
   attributed to a scope it was not declared in, and a module-scope `my atomicint` is visible
   from an exported routine.
2. **A declared native target costs nothing.** `compiler/atomic_target.rs` emits no guard for
   it; `$n⚛++` in a hot loop compiles as it did.
3. **Anything else gets a pass-through guard**, `__mutsu_atomic_int_target(target, declared,
   spelling[, operand])`, emitted between pushing the target name and the helper call; it
   answers its operand so the stack is untouched. `declared` is the declared type for a
   declaration that is not native (always refuses), or `Nil` when no declaration decides --
   a parameter that passes a container through, an attribute, an element, a `:=` alias, a name
   from a scope the compiler does not see (method bodies are compiled without the enclosing
   scopes; `EVAL`). Those ask the container at run time.
4. **One check, one predicate.** `Interpreter::builtin_atomic_int_target`
   (`runtime/builtins_atomic_target.rs`) is the only place a target is judged, and
   `native_types::is_atomic_int_target_type` the only definition of "native-integer type" (the
   signed family: `int`, `atomicint`, `int8`..`int64`, `long`, ...; unsigned, `num`, `str` and
   every boxed type are refused). The nine copy-pasted atomic routine lowerings in
   `compiler/expr_call.rs` became one table-driven `compile_int_atomic_var_call`.
5. **The dynamic verdict is lenient on silence.** An attribute is judged by its class
   declaration, an element by its array's element type (a hash element is always refused),
   a scalar by a recorded type -- the name-keyed one or its cell's constraint. A cell
   *without* a constraint answers nothing: cells are made at ~35 sites and not all copy the
   declaring variable's type, so `my atomicint $x; my $y := $x; $y⚛++` reaches one and must run.
6. **The operator's spelling is its routine name.** The parser lowers each integer-atomic
   operator to a call of Rakudo's own routine for it (`postfix:<⚛++>($x)`,
   `prefix:<--⚛>(@a[0])`, `infix:<⚛+=>($x, $n)`), so a refusal names the operator as the program
   wrote it, and the AST stays an ordinary call that round-trips through RakuAST. (An internal
   `__mutsu_*` marker would not: the RakuAST converter refuses every `__`-prefixed desugaring
   marker, which took `t/concurrency/thread-lock/atomic-element-targets.t` out of the
   `ci/rakuast-frontend-passing.txt` ratchet in the first version of this change.) The
   `nqp::` `_i` forms are marked integer-only by the compiler while it lowers them, which is what
   makes `nqp::atomicload_i($plain)` refuse while `nqp::atomicload($plain)` stays legal.
7. **`cas` is not an integer atomic.** `cas($x, * + n)` lowered to `__mutsu_atomic_add_var`; it
   now lowers to an unchecked `__mutsu_cas_add_var` alias.
8. **TRIR declines a routine that calls an atomic.** An atomic addresses its target by name
   (a shared cell, an attribute, the name-keyed lane); a TRIR frame slot is none of those, and
   a TRIR call by name sees only the value, which would also bypass the guard.

## 3. Rejected alternatives

- **A name-keyed run-time check alone** (the dropped first attempt). Refuses a real `atomicint`
  whose metadata is out of reach; see §1.
- **Refuse whenever the container has no native marker.** Needs every cell promotion site to
  record the declaring type (or an explicit "declared plain" mark), which is the real fix and
  is #12007. Until then it would reject the `:=` alias above.
- **Always put the declared type on every cell at declaration.** Moves a measurable cost onto
  every native lexical (`my int $i` loops are hot) to serve a diagnostic.
- **A parallel table of declared types beside `local_scopes`.** The frames are cloned, parked
  (`pending_scope_frame`) and replaced by hand in `control_for.rs`; a table kept in lock-step
  with them can drift, and a misaligned entry is a false refusal.
- **An operand-carrying tail on each helper instead of a guard call.** Two code paths per
  helper, and the message needs the operand's type anyway.

## 4. Consequences and known gaps

- Every `thread-lock/*.t` and every whitelisted roast file that touches atomics passes
  unchanged: none relied on the leniency.
- The check is conservative by construction. Not refused yet (all still run, as before):
  an untyped variable passed to an `is rw` parameter ([#12007](https://github.com/tokuhirom/mutsu/issues/12007)),
  and a pointy block's parameter with no `ParamDef`. (MoarVM's "not of the machine's native size"
  refusal of `int8`/`int16`/`int32` was a known gap here; §5 records its closing.)
- Found on the way: an inner-block `my atomicint $y` shadowing an outer `$y` is ignored by the
  by-name read-modify-write ([#12006](https://github.com/tokuhirom/mutsu/issues/12006));
  `@a[0] ⚛+= n` is a parse error ([#12005](https://github.com/tokuhirom/mutsu/issues/12005)).
- When #12007 lands (the native kind on the container), the dynamic verdict for a cell becomes
  a read of that kind and the "lenient on silence" rule in §2.5 can be tightened; the lexical
  classification stays, since it is what Rakudo does and costs nothing at run time.

## 5. Amendment (2026-10-06): a narrow native integer is refused by every atomic

[#12008](https://github.com/tokuhirom/mutsu/issues/12008). Rakudo's `atomicint $target is rw`
candidate takes an `int8` / `int16` / `int32` (and `bool`) container; MoarVM then refuses **every**
atomic operation on it, the lenient ones (`atomic-fetch`, `atomic-assign`, `cas`, `⚛$x`, `$x ⚛= v`)
included, and words the refusal by where the container lives (always `X::AdHoc`, whatever the
operation's spelling, `nqp::` `_i` ops included):

| target | message |
| --- | --- |
| `my int8/int16/int32 $x`, a parameter, an alias | `Cannot atomic load from an integer lexical not of the machine's native size` |
| element of `my int8/int16/int32 @a` | `Can only do integer atomic operation on native integer array element of atomic size` |
| `has int8/int16/int32 $!v` | `Can only do an atomic integer operation on an atomicint attribute` |

- **Two predicates.** `native_types::is_atomic_int_target_type` is now the machine-size family only
  (`int`, `atomicint`, `int64`, `long`, `longlong`, `ssize_t`); `is_narrow_atomic_int_type` is
  `int8`/`int16`/`int32`/`bool`. §2.4's "one predicate" becomes "one pair": the dynamic verdict
  (`Verdict::Native | Narrow | Boxed | Unknown`) is still the only place a target is judged.
- **Strict forms need no new compiler path.** A declared narrow type is no longer "proven native", so
  `emit_int_atomic_guard` emits the guard with the declared type and the verdict raises the narrow
  message; an undecided target asks the container, as before.
- **Lenient forms are judged where their target type is already read.** A separate guard call per
  `cas` / `atomic-fetch` / `atomic-assign` cost a `cas` loop on an attribute ~50% and on an element
  ~20% (debug A/B against `main`), so the three target kinds differ:
  - a *scalar* the compiler sees declared narrow gets the always-refusing
    `__mutsu_atomic_narrow_target(target, declared)` guard (`emit_narrow_atomic_guard`); a scalar no
    declaration decides (a parameter, an outer name) gets the same guard asking the container at run
    time; a scalar declared plain or machine-size emits nothing, so `my atomicint $n; ⚛$n` and the
    hot `cas($x, ...)` loop compile as before;
  - an *array element* is judged by `check_atomic_elem_type` (every store / `cas` already reads the
    container's element type there) and, for the one op that reads none, `fetch`, by one extra
    `atomic_elem_type_constraint`;
  - an *attribute* is judged by `refuse_narrow_attribute` (the ADR-0121 memoized
    `self_attr_type_constraint`, the lookup an ordinary `$!x = v` already pays), called from
    `atomic_assign_coerced_value` (store, `cas`) and the attribute branch of the fetch.

  Release A/B against `main` (one thread, 400000 `cas` iterations per path, 5 interleaved rounds,
  medians): attribute 3.82 s -> 3.94 s, element 3.29 s -> 3.44 s, and the unchanged lexical control
  3.03 s -> 3.17 s, i.e. within the control's own +-5% noise. (A debug build shows ~+19% on the
  attribute path, an artifact: `self_attr_type_constraint` re-derives every memo hit under
  `debug_assert_eq!`.) The by-name lexical metadata is deliberately *not* consulted at run time for a
  scalar: under shadowing it could name an outer variable, which would be a false refusal.
- **`@a[0] ⚛= v` was a plain assignment.** Both the statement and the expression parser built an
  `IndexAssign`, so an element `⚛=` was neither atomic nor guarded. It now lowers to
  `atomic-assign(@a[0], v)`, which the element-atomic compiler already maps onto the element's cell.
- **The code form refuses before its block runs.** `cas($!narrow, { ... })` and `cas(@narrow[0],
  { ... })` check once up front (`refuse_narrow_attribute` / `refuse_narrow_element`), as Rakudo
  refuses at the first atomic load; `t/concurrency/thread-lock/atomic-narrow-int-refused.t` pins that
  the block ran zero times.
- **Not modelled**: the non-`_i` `nqp::` ops on a narrow container (`nqp::atomicload($int32)`) keep
  running; MoarVM refuses them with a different message (`A IntLexRef container does not know how to
  do an atomic load`), which also applies to a machine-size `int` and is a separate gap.
