# `for LIST -> \x, $value { }` does not write through to the source list's variable elements

## Symptom

A `for` loop over a list of variables, with a sigilless pointy-block
parameter (`-> \x`), is supposed to alias each iteration's `\x` to the
corresponding element of the source list when that element is itself an
lvalue (a variable). Writing to `x` inside the loop body should write through
to the source variable, exactly like a sigilless `:=` bind or a sigilless
routine parameter. In mutsu this write-through does not happen AT ALL — not
even for untyped variables:

```raku
my $a;
my $b;
for $a, 1_000, $b, 1_000_000 -> \x, $value {
    x = $value;
}
say "a=$a b=$b";
```

Raku: `a=1000 b=1000000`.

mutsu: prints two "Use of uninitialized value ... in string context" warnings
(from the `say` string-interpolating `$a`/`$b`, which are still unset) and
then `a= b=` — the write to `x` inside the loop body never reaches `$a`/`$b`
at all.

## Why this matters

This is the actual shape `Native::Overflow`'s test suite exercises
(`t/01-basic.rakutest`, see
`todo/tickets/dist-test-suite-failures-batch.md`'s un-triaged `test_die` row),
and was originally conflated with the type-check-skip bug fixed in
`news/2026-08/sigilless-alias-write-now-type-checked.md`. It is NOT the same
bug: the type-check fix does not help here because the write-through itself
is missing, independent of any type constraint. The subset/type-check
question (does `x = $value` correctly raise `X::TypeCheck::Assignment` when
`$value` doesn't fit `$a`'s declared type) is unreachable for this repro
until the write-through is fixed first.

## Root cause (not yet investigated)

Compare with the two shapes that DO work today:

- `my \x := $a; x = v;` (a `:=` bind) — writes through via the
  `__mutsu_sigilless_alias::` forward chain (see
  `src/vm/vm_var_assign_set_local.rs`, `resolve_sigilless_alias_source_name` /
  the bind-time alias registration around line 1306-1340).
- `sub f(\x) { x = v }; f($a);` (a sigilless routine parameter) — same
  mechanism, registered at call/parameter-bind time.

A `for LIST -> \x, $value { }` pointy-block parameter is a DIFFERENT bind
shape at the AST/compiler level (a loop-parameter bind, not a `Stmt::VarDecl`
with `AssignOp::Bind` or a routine `ParamDef`), so it is very likely compiled
through a different code path in `src/compiler/stmt.rs` / the `for`-loop
compilation helpers (`src/compiler/helpers_phasers.rs`,
`src/vm/vm_for_loop_body.rs`, `src/vm/vm_for_loop_intrange.rs`) that never
registers the `__mutsu_sigilless_alias::x -> <source-element-name>` mapping
at all. Grep `t/for-sigilless-rw.t` and `t/bind-array-elem-in-loop.t` for
existing coverage of adjacent shapes before starting.

**Confirmed via `t/for-sigilless-rw.t`**: `for @array -> \v { }` (a single
array's elements as the source), `for %hash.kv -> \k, \v { }`, and
`for @array.values -> \v { }` ALL already write back correctly, including
`++`/`--` — that file's 17 assertions all pass today. The gap is narrower
than "for-loop sigilless write-through in general": it is specifically a
`for` source that is a comma-separated LIST OF SEVERAL DISTINCT SCALAR
VARIABLES (`for $a, $b -> \x, \y { }`), as opposed to a single container's
elements. Whatever compiles/executes the array-element case evidently
registers a working alias/writeback per iteration; the multi-variable-list
case needs the same treatment, likely in the general (non-array-source) `for`
loop compilation path — start by comparing how `Stmt::For` compiles when its
source expression is an `Expr::List`/comma-list of plain `Expr::Var`s versus
a single `Expr::ArrayVar`.

## Minimal repro

```raku
my $a;
my $b;
for $a, 1_000, $b, 1_000_000 -> \x, $value {
    x = $value;
}
say "a=$a b=$b";   # raku: a=1000 b=1000000; mutsu: a= b= (no write-through at all)
```

The `Native::Overflow`-shaped repro (with a `subset`/native-type element,
once the write-through above is fixed, needs the type-check to ALSO fire):

```raku
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a;
my SmallInt $b;
for $a, 1_000, $b, 1_000_000 -> \x, $value {
    CATCH { say "caught: {.^name}"; next }
    x = $value;   # should throw X::TypeCheck::Assignment for both iterations
}
```
