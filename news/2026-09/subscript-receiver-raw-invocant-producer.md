# A subscript receiver hands its element's container to a raw invocant

`@a[0].mut` and `%h<a>.mut`, where `mut` declares a raw invocant, mutate the
element in raku. mutsu silently did nothing:

```raku
use v6.e.PREVIEW; use MONKEY-TYPING;
augment class Int { method mut(\S:) { S = 7 } }
my @a = 1, 2;   @a[0].mut;  say @a;   # raku: [7 2]   was: [1 2]
my %h = a => 1; %h<a>.mut;  say %h;   # raku: {a => 7} was: {a => 1}
```

These are rows I3 and K3 of
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md),
the last slice that ADR left open. With this the ADR is **Accepted**: every
slice is implemented.

## Why slice 3b could not reach them

Slice 3b made a raw invocant parameter bind the caller's container by boxing the
receiver's storage *location* and arming a one-slot channel the parameter binder
consumes. It could only do that for a receiver the call site can **name**:
`CallMethodMut` carries a `target_name_idx`, and `capture_lvalue_invocant_cell`
turns that name into a location.

A subscript receiver has neither half. `--dump-bytecode` on `@a[0].mut` is

```text
GetArrayVar(0); LoadConst(3); Index { is_positional: true }; CallMethod { .. }
```

— a plain `CallMethod` with no name at all, whose receiver is the *value* the
`Index` op has already read out of the array. There is nothing left to box.

## The fix is a producer, and two thirds of it already existed

The missing piece is the subscript handing over the element's own `Scalar`
container instead of its value. What made that a small change rather than the
campaign the ticket feared is that the two mechanisms the ticket said would have
to be built were already in the tree:

- **The decontainerize chokepoint.** `exec_call_method_op_impl` has
  decontainerized a `ContainerRef` invocant since ADR-0036 slice 3 started
  handing elements out in bulk, with `.VAR` and the renderers as its documented
  exceptions. So the ~40 `Instance`/`Array`/`Hash` dispatch branches below it
  never see a container, and no new guard was needed — the hazard slice 3a had
  to solve at its own site does not arise here.
- **The consumer.** Slice 3b's `arm_raw_invocant_arrival` already prefers an
  existing `ContainerRef` receiver over minting a cell. The only edit that reuse
  needed was to lift its `target_name.is_empty()` pre-gate: the arming site now
  takes two stack positions rather than a name, because `CallMethod` is
  `[receiver, args..]` while `CallMethodDynamic` puts the runtime method name
  between them.

So what shipped is one new opcode and one new file:
`OpCode::IndexInvocantRef`, emitted by `mark_trailing_index_as_invocant_ref` in
place of the receiver's trailing `Index`, and `src/vm/vm_subscript_invocant_ref.rs`,
which hands out `array_slot_ref` / `hash_slot_ref` — the same in-place,
idempotent promotion `.pairs`/`.values`/`.Seq` already use.

**The producer declines by leaving the stack untouched**, which is what keeps it
from re-deriving the subscript machinery: slices, `Whatever`, junction and
`Range` receivers, `postcircumfix` overloads, missing keys and past-the-end
indices all fall through to the one `Index` implementation that already handles
them. `List`/`ItemList`/`Lazy` receivers are excluded deliberately — promoting
an immutable list element would turn raku's refusal into a silent success, which
is strictly worse than the silent no-op it is today
(`todo/tickets/immutable-list-element-write-is-silently-dropped.md`).

## Unconditional emission, a one-atomic-load gate

Rawness is not statically knowable — it depends on the element's runtime type,
and for `@a[0]."$name"()` on a runtime string — so the compiler emits the
producer for every `<subscript>.method(...)`, one of the most common shapes
there is. What keeps that affordable is slice 3b's set-only process-global
mirror: in a program that declares no raw-invocant method anywhere,
`IndexInvocantRef` is one relaxed atomic load followed by `Index`, and the
`CallMethod` arming site is one relaxed atomic load taken before the method name
is even read from the constant pool.

The JIT needed no separate treatment, which the ticket flagged as a risk:
`vm_jit_support.rs`'s shim list already carries `OpCode::Index`, so neither op
is compiled to native code and both run the same interpreter arm. The pin drives
a 200-iteration loop past the default threshold of 100, and was additionally run
under `MUTSU_JIT_THRESHOLD=1` and `MUTSU_JIT=off` — 33/33 in all three.

## One duplicate collapsed on the way

`@a[*-1].mut` needs the `*` resolved before there is an index to address, and
that resolution existed **twice** — inline in the single-dimension slice walk
and again in `vm_var_multidim_ops.rs`. Both now call one
`Interpreter::eval_whatever_code_index`, which is also what the producer calls:
three consumers, one implementation, per PLAN.md's standing rule.

## What the neighbourhood survey turned up

Beyond I3/K3, all of these went from silently wrong to correct: nested
subscripts in all four combinations (`@a[0][1]`, `%h<a>[0]`, `%h<a><b>`,
`$obj.attr[0]`), the runtime method-name spelling, `@a[*-1]`, computed
subscripts (evaluated exactly once), shaped arrays, typed arrays — whose element
constraint rides on the promoted cell — and role-composed raw invocants.

The survey also found the immutable-`List` gap above, which is the
readonly-enforcement family ADR-0067 records as rows L4/L5/M1/M2 rather than
anything this slice introduced.

## Pinned by

`t/raw-invocant-subscript-receiver.t` — 33 tests, byte-identical output under
`mutsu` and `raku`. Half the file is deliberately regression material: the
`augment` at the top raises the program-wide flag, so every ordinary method call
over a subscript receiver in it (`.succ`, `.WHAT`, `.uc`, `.push`/`.append` on
container elements, `.raku`/`.gist`/`.Str` rendering after a promotion,
`.sort`/`.grep`/`.sum`, and copy semantics for an array and a hash) is
exercising the producer's decline path and the chokepoint that hides the
container from an ordinary callee.
