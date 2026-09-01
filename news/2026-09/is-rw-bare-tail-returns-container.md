# An `is rw` routine's bare tail returns its container (ADR-0059 Slice 2 closed)

Closes `todo/deep/is-rw-sub-implicit-return-element-not-mutable.md` (found by
the doc-diff harness on `Type/Routine.rakudoc:231`, TRIAGE Tier S on
2026-09-01) and the bare-tail half of
[ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md) Slice 2.

## What was wrong

An `is rw` routine whose last statement is a plain expression — no
`return-rw` — hands raku its caller a *container*. mutsu compiled that tail
as a value read, and `f() = v` then fell back to a caller-side
re-interpretation of the callee's AST tail (`rw_sub_target_expr` /
`assign_rw_target_expr`): the tail expression was re-evaluated *in the
caller's frame*. That happens to work for a bare outer lexical and is
structurally wrong for everything else, most visibly a location reached
through the routine's own parameter:

```raku
sub walk(%h) is rw { %h<some> }
my %hash = some => 1;
walk(%hash) = "val";
say %hash<some>;     # raku: val   mutsu: 1  -- the write silently vanished
```

The caller's frame has no `%h`, so the re-interpretation resolved nothing
and the assignment was discarded with exit 0. The array twin
(`sub walk(@a) is rw { @a[0] }`) and the doc's recursive `walk` (a `:=`-walked
path returned as `$current`) lost their writes the same way.

## What changed

**The compiler now compiles the bare tail of an `is rw` / `is raw` routine
exactly like a `return-rw` operand** (`Compiler::rw_tail`, consumed by
`compile_routine_tail_expr`, which routes to `compile_return_rw_arg`): a
subscript yields the element's shared `ContainerRef` cell (or the deferred
`HashEntryRef` token for a missing key, so a getter built on the same routine
still does not vivify), a plain scalar lexical yields its own cell
(`WrapVarRef` + `CaptureVarCell`), a nested call in the tail gets
container-mode arguments, and — new — a ternary tail compiles each arm in
container mode so `$flag ?? c<x> !! c<y>` assigns through the taken branch.
The flag is set for named subs, methods, anonymous `sub ... is rw`, and the
interpreter's carrier recompile of a code object (`pending_eval_rw_tail`, taken
at `eval_block_value_inner` entry so it never leaks into a block compiled from
inside the body); it is *not* set for a pointy/bare block whose `is_rw` is a
loop-parameter trait.

**The runtime always runs the routine and writes through what it returns.**
`assign_named_sub_lvalue_with_values` / `assign_callable_lvalue_with_values`
no longer inspect the callee's body: a routine is rw-capable when it is
declared `is rw`/`is raw` or its body spells `return-rw` anywhere
(`RoutineBodyFacts::uses_return_rw`, a plan-lowered fact like
`declares_state`), and for such a routine the result goes through
`assign_lvalue_container`; a plain value there is
`X::Assignment::RO: Cannot modify an immutable Int (42)`, raku's wording. A
routine that is not rw-capable still runs (raku evaluates `h()` before
rejecting `h() = 1`) and is refused even when it happens to return a `Proxy`.
`++f()` / `f()++` use the same capability rule.

**Deleted:** `rw_sub_target_expr`, `is_explicit_return_rw_target`,
`assign_rw_target_expr`, the `rw_tail_expr` field on `FunctionDef` and
`CompiledRoutineMetadata`, and their eleven initializers. There is now one
rule with no stated gap: the container return owns every location a routine
hands back.

## Four general fixes this surfaced

- **`$m.return-rw`** (the method spelling) compiles as `return-rw $m`
  instead of dispatching a method with a decontainerized invocant — the old
  re-interpretation had special-cased it by name (`t/lvalue-sub-plan-tail.t`).
- **A captured slot holding a deferred entry token is handed out as-is** by
  `CaptureVarCell` (`capture_var_cell_inner` and the env-resident fallback)
  rather than boxed into a fresh cell, so `my $current := thing{$k}; ...;
  $current` returns the entry and the write autovivifies the path.
- **A typed array/hash element's constraint rides on its promoted cell**:
  `array_slot_ref` / `hash_slot_ref` register `value_type` on the cell they
  create and `assign_lvalue_container` checks it, so `tel() = "nope"` on a
  `my Int @typed` element is `X::TypeCheck::Assignment` as `@typed[0] = "nope"`
  is. This is the core of ADR-0036 slice 4; two pins that were `todo`-marked
  for it now pass and are un-marked (`t/for-loop-element-alias.t` row 28,
  `t/subscript-pair-element-container.t` row 12).
- **A return-type constraint reads through the container** it checks
  (`enforce_return_type_constraint` checks `value.deref_container()` and
  returns `value`), so `sub foo(--> Callable:D) is rw { my $x is default(Nil)
  = Nil; $x }` passes as in raku (`roast/S06-advanced/return.t` 109), and a
  method call on a deferred entry token straight off a call
  (`g().defined`, `g().raku`) reads the entry's current value.

## Verification

`t/is-rw-bare-tail-returns-container.t` (32 assertions, identical output
under `raku` v2026.06): parameter-reached hash/array elements, the doc's
recursive walk, bare-variable / `is raw` / rw-parameter tails with `=`,
`+=`, `++`, and a `:=` binding shared in both directions, the non-vivifying
getter, the ternary tail, method element and attribute tails, a body with
`CATCH`, an anonymous `sub () is rw`, `return-rw` without the trait, the
three non-assignable shapes, and container invisibility in rvalue context.
Existing pins `t/is-rw-lvalue-container-return.t`,
`t/return-rw-container-values.t`, `t/return-rw.t`, `t/lvalue-sub-plan-tail.t`
are unchanged and green; the 105 whitelisted roast files that use `is rw` /
`return-rw` / `is raw` pass.

## Residue

- `.pairs` routing (ADR-0036 slice 3) and the compensator deletion (slice 4's
  second half) are untouched.
- ADR-0059 Slice 3 (container mode for every single-dimension subscript call
  argument) stays open.
