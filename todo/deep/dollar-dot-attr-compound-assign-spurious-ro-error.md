# A `$.attr` compound assign should be a silent no-op, not a refusal

> **Moved from `todo/tickets/` to `todo/deep/` on 2026-08-26**, because the fix it
> asked for was a semantic change to what *every* public accessor returns. That is
> **no longer** the shape of the work — see "What actually has to change" below.

**Every previous diagnosis in this file, and TRIAGE's correction of it, was wrong
about the mutsu column.** The 2026-09-01 tail said `$.x *= 2` mutates; the
2026-09-06 TRIAGE said it throws. Both are right, for *different statement
positions*, and neither noticed the other. The full table below was measured on
`main` at `710b21d9c` (2026-09-06) against `raku` v2026.07, and every row was run
both bare and inside `try`/`CATCH`.

## Status 2026-09-06: the silent half is fixed, the loud half remains

The **simple** assignment now behaves like raku:
`$.x = 9` on a non-`rw` scalar accessor throws
`X::Assignment::RO: Cannot modify an immutable Int (5)` and leaves the attribute
alone, in statement and expression position alike
(`news/2026-09/dot-twigil-accessor-assignment-consults-rw.md`,
`t/dot-twigil-accessor-readonly.t`). Silent over-mutation is gone.

What is left is narrower and is **not** silent data loss: a `$.x OP= v` compound
assign on a non-`rw` scalar accessor now **refuses** in every position, where raku
silently discards the write and yields the computed value. That is internally
consistent and safe, but it is still a divergence, and the rest of this file is
the map for closing it. The original title's "spurious RO error" is now the
*only* remaining symptom, which is why the file has been retitled.

## The measured table

For a **non-`rw` scalar** `has $.x = 5`, inside a method of that class:

| form | raku | mutsu before | mutsu now |
|---|---|---|---|
| `my $r = ($.x *= 2)` | `r=10`, attr stays `5` | `r=10`, attr became `10` | **throws** |
| `my $r = $.x *= 2` | `r=10`, attr stays `5` | `r=10`, attr became `10` | **throws** |
| `($.x *= 2);` as a statement | attr stays `5` | attr became `10` | **throws** |
| `$.x *= 2;` as a bare statement | attr stays `5` | throws `method 'x' is not rw` | throws (same) |
| `$.x = 9;` | throws `Cannot modify an immutable Int (5)` | attr became `9` | **throws, same message** |
| `my $r = ($.x = 9)` | throws | `r=9`, attr became `9` | **throws, same message** |
| `self.x *= 2` | throws | throws (`method 'x' is not rw`) | unchanged |
| `self.x = 9` | throws | throws (`method 'x' is not rw`) | unchanged |
| `$!x *= 2` | mutates | mutates | unchanged |

Everything else already agrees and must not be disturbed. Measured on the same
run: `$.x.VAR.^name` / `self.x.VAR.^name` / `F.new.x.VAR.^name` are
`Scalar` / `Int` / `Int` in both; `has $.y is rw` mutates for every form in both;
and for a **non-`rw` `@.a` / `%.h`**, `@.a.push(3)`, `@.a = 7,8`, `@.a[0] = 99`,
`%.h<k> = 99` all succeed in raku *and* in mutsu. **Only the `$`-sigil accessor
throws in raku**, because `@.a`'s accessor hands back the Array container and
assigning into a container is `STORE`, not a modification of an immutable value.

## The mechanism, traced through `--dump-ast`

`$.x` reaches the store through **two different lowerings**, which is why the
divergence looks self-contradictory:

1. **Expression position** (`my $r = ($.x *= 2)`, `($.x *= 2);`) keeps
   `CompoundAssign { target: Var(".x"), expanded: AssignExpr { name: ".x", … } }`.
   `AssignExpr { name: ".x" }` is compiled by `Compiler::compile_expr_assign`
   (`src/compiler/expr_data.rs:44-59`), whose `$.attr` arm merely **validates that
   the accessor exists** — `CallMethod(x)` followed by `Pop` — and then performs an
   ordinary *named* assignment to the variable `.x`, which writes the attribute
   directly. No rw-ness is ever consulted. That is the whole reason `$.x = 9` and
   the expression-position compound both over-mutate.
2. **Bare statement position** (`$.x *= 2;`) is rewritten by the parser into
   `MethodCall { target: Var("__ANON_STATE__"), name: "x" }` and lowered to
   `__mutsu_assign_method_lvalue(self, "x", [], <computed>, "__ANON_STATE__", true)`
   (`src/parser/stmt/assign/compound_expr.rs:308-338` → `assign/lvalue.rs`). That
   reaches `assign_method_lvalue_with_values`
   (`src/runtime/methods_mut_method_lvalue.rs:1076-1092`), which rejects a
   non-`rw` public **scalar** accessor with "method '…' is not rw". Correct to
   refuse a `self.x` write; wrong for `$.x`, which raku itemizes.

So `$.` and `self.` are indistinguishable by the time the statement path reaches
the runtime — the `__ANON_STATE__` collapse in
`src/parser/stmt/assign/assign_stmt.rs:62-65` is deliberate and drops exactly the
distinction the fix needs.

## Why raku behaves the way it does

`$.x` inside a method is `self.x` **itemized**: the `$` sigil puts the accessor
result in item context, which for a bare value is a *fresh anonymous `Scalar`*.
`self.x` is not itemized.

- non-`rw`: the accessor returns a bare value, so `$.x` itemizes it into a
  throwaway `Scalar`. `$.x *= 2` assigns into the throwaway — expression value
  `10`, attribute unchanged, **no exception**.
- `is rw`: the accessor returns the attribute's real `Scalar`, itemization is the
  identity, and `$.x *= 2` genuinely mutates.
- A **simple** assignment is compiled as an lvalue *without* the itemize wrapper
  (it would defeat the assignment), so it hits the raw accessor return and throws.

## What actually has to change

Earlier passes concluded that step 1 had to be "make an `is rw` accessor return
its `Scalar` container", and deferred three times on that ground. **That is not
required.** Every `is rw` form already behaves correctly in mutsu, and so does
every `@.`/`%.` form. The work is confined to the `$`-sigil non-`rw` case and to
teaching the two lowerings apart:

1. ~~**`AssignExpr { name: ".attr" }` must consult rw-ness.**~~ **DONE.** The
   check went in at the store (`Interpreter::check_dot_twigil_accessor_writable`,
   `src/vm/vm_misc_assign.rs`) rather than in the compiler, because rw-ness is a
   property of the *invocant's class* and only the runtime knows it. It is scoped
   to the `$` sigil: measured, raku accepts `@.a = 7,8` and `%.h<k> = 99` on
   non-`rw` container attributes.
2. **A `$.`-twigil compound assign must become a silent no-op for a non-`rw`
   scalar accessor**, yielding the computed value. This is all that is left, and
   both lowerings need it:
   - expression position: intercept `Expr::CompoundAssign { target: Var(".x"), .. }`
     in `Compiler::compile_expr` (`src/compiler/expr.rs:785`, which today just
     compiles `expanded`) before change 1 turns it into a throw;
   - statement position: carry the `$.`-origin through
     `method_lvalue_roundtrip_assign_expr` as a new argument of
     `__mutsu_assign_method_lvalue` (arg 5 is `target_var`, arg 6 is
     `preserve_hash_entries`; add arg 7), read it in
     `builtin_assign_method_lvalue` (`src/runtime/builtins_multidim_assign.rs:621`)
     and thread it into `assign_method_lvalue_with_values`, where the non-`rw`
     public scalar rejection returns `Ok(value)` instead of an error.
3. ~~**Align the message.**~~ **DONE for the `$.` form** — it now reports
   `X::Assignment::RO: Cannot modify an immutable Int (5)`. `self.x = 9` and
   `self.x *= 2` still say `method 'x' is not rw`; raku uses the immutable-value
   wording there too. Cheap to align at
   `src/runtime/methods_mut_method_lvalue.rs:1076-1092`, but do it in the same
   change as item 2 so the two do not disagree about which message a `$.` write
   gets.

Why item 2 could not simply be lowered as `do { my $t = self.x; $t OP= v; $t }`,
which would be raku's model literally: mutsu's `is rw` accessor hands back a
*value*, not the attribute's `Scalar` container, so a throwaway temporary would
break every `is rw` compound assign — all of which pass today. Making the accessor
return its container is the change earlier passes of this file proposed; it is
still the principled route, but item 2 does not require it, and nothing else in the
measured table does either.

## Blast radius (measured when item 1 landed)

Zero, in the end. Of the 138 `t/` files whose crude grep matched `$.name =`, all
but two were `has $.x = default` declarations or `is rw` attributes. Of those two,
`t/iterable-iterator-protocol.t:62` writes an `is rw` attribute and
`t/imperative-does-parameterized-role.t:9` writes from a `sub` (no invocant, so
the guard does not fire). `make test` passed unchanged.

## Affected files

- `src/compiler/expr_data.rs` — `compile_expr_assign`'s `$.attr` arm (change 1)
- `src/compiler/expr.rs:785` — the `CompoundAssign` arm (change 2, expression half)
- `src/parser/stmt/assign/lvalue.rs`, `compound_expr.rs`, `assign_stmt.rs` —
  carrying the `$.`-origin (change 2, statement half)
- `src/runtime/builtins_multidim_assign.rs:621` — `builtin_assign_method_lvalue`
- `src/runtime/methods_mut_method_lvalue.rs:1076-1092` — the rejection site
