# Assigning through a sigilless `\x := $var` alias skips the target's type constraint

## Symptom

`Native::Overflow`'s test suite (`t/01-basic.rakutest`, un-triaged `test_die`
row in
[todo/tickets/dist-test-suite-failures-batch.md](../tickets/dist-test-suite-failures-batch.md))
plans 30 tests but runs 0 — every assertion lives inside a `CATCH` block that
never fires, because the type-check exception it expects never gets thrown.

Minimal repro, independent of the dist:

```raku
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a = 5;
my \x := $a;
x = 1000;
say $a;
```

Raku: `Type check failed in assignment to $a; expected SmallInt but got Int
(1000)`.

mutsu: prints `1000` — the assignment silently succeeds, the subset's `where`
constraint is never checked.

A **direct** assignment to the same variable (`$a = 1000;`, no alias) IS
correctly type-checked in mutsu — confirmed working via
`t/nil-uint-smartmatch.t`-adjacent testing this session and
`roast/S32-num/int.t` (165/165 passing). The gap is specific to writing
**through a sigilless bind alias**.

## Root cause (partial — compile-time half confirmed, runtime half not investigated)

The compiler emits an `OpCode::TypeCheck` before a variable's store based on
`self.local_types: HashMap<String, String>`, populated at each `my TYPE
$var` declaration site (`src/compiler/stmt.rs:1316`,
`src/compiler/helpers_block_inline.rs:284`) — keyed by the **declared
variable's own name**.

`my \x := $a;` declares a NEW sigilless name `x` with no type annotation of
its own (the AST's `type_constraint` field is `None` for this decl) — so
`local_types` never gets an entry for `"x"`, even though `$a` (the bind
target) has one (`"a" -> "SmallInt"`). When `x = 1000;` is later compiled,
there is no `local_types["x"]` to consult, so no `TypeCheck` opcode is
emitted at all for this store.

## Why this needs a design pass, not a quick patch

A **narrow** fix — when compiling a sigilless `:=` bind whose RHS is a
simple `Expr::Var(name)`, copy `local_types.get(name)` into
`local_types[alias]` too — would only cover the same-compile-unit, statically
resolvable case (exactly this repro, and the dist's `for LIST -> \x, $value
{ }` shape, since `x` binds to `$a`/`$b`, both plain locals with a static
constraint). It would NOT cover:

- Binding across a function boundary (`sub f(\x) { x = 1000 }; f($a);` — `x`
  is a parameter alias, `$a`'s type is not visible at `f`'s compile time at
  all).
- Binding to a computed/indexed target (`\x := @arr[$i]`) where the element's
  type constraint (for a typed array) is dynamic.
- Any case where the alias crosses an `EVAL`/closure boundary.

The architecturally correct fix is for the **container itself** to carry its
type constraint at runtime (so any alias reaching the same container is
checked, regardless of what name compiled it), not a compile-time
name-keyed map. This is the same shape of gap the `ContainerRef`/interior-
mutability work already targets (see ADR-0013 §7, "GcBox/UnsafeCell interior
mutability refinement") — worth checking whether that machinery already
carries (or could easily carry) a type-constraint field before implementing
a separate mechanism.

## Repro

```raku
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a = 5;
my \x := $a;
x = 1000;
say $a;   # raku: dies at the assignment; mutsu: prints 1000
```

The dist's own shape (also confirmed failing, same root cause):

```raku
use Native::Overflow;   # lexically shadows int8/int16/... with `subset ... where ...`
my int8 $a;
my int16 $b;
for $a, 1_000, $b, 1_000_000 -> \x, $value {
    CATCH { say "caught: {.^name}"; next }
    x = $value;   # should throw X::TypeCheck::Assignment; silently succeeds in mutsu
}
```
