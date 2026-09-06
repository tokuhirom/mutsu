# `$.attr .= meth` is the last `$.`-twigil RMW that still refuses

> Renamed from `dollar-dot-attr-compound-assign-spurious-ro-error.md` on
> 2026-09-07, when everything else in that file landed. History: it started in
> `todo/tickets/`, moved to `todo/deep/` on 2026-08-26 because it looked like a
> semantic change to what every public accessor returns, and was wrong about the
> mutsu column three separate times. See
> `news/2026-09/dot-twigil-accessor-assignment-consults-rw.md` (the simple `=`)
> and `news/2026-09/dot-twigil-accessor-rmw-is-a-noop.md` (everything else).

## What is closed

`$.attr` inside a method is `self.attr` **itemized**. For a non-`rw` scalar
accessor the accessor hands back a bare value, so the itemization is a fresh
throwaway `Scalar`, and every **read-modify-write** assigns into that throwaway:
the expression yields the computed value, the attribute is unchanged, and
nothing is thrown. Only the simple `$.x = v` is compiled without the itemize
wrapper, so it alone hits the raw accessor return and dies `X::Assignment::RO`.

All of that now matches raku v2026.07, across three entry points:

- `AssignExpr(name_idx, dot_twigil_rmw)` — expression-position `OP=`;
- `__mutsu_assign_method_lvalue` — statement-position `OP=`, including
  `min=` / `//=` / `orelse=`;
- `try_slotless_attr_incdec` — `$.x++`, `++$.x`, `$.x--`, `--$.x`.

Pinned by `t/dot-twigil-accessor-readonly-rmw.t` (17 rows, green under raku).

**Two claims in the old file were wrong, and cost the earlier passes:**

1. "`$.` and `self.` are indistinguishable by the time the statement path reaches
   the runtime — the `__ANON_STATE__` collapse drops exactly the distinction the
   fix needs", with a proposed fix of threading a 7th argument through
   `__mutsu_assign_method_lvalue`. **False.** The collapse *preserves* the
   distinction on purpose: `assign_stmt.rs` keeps the collapsed
   `__ANON_STATE__` spelling precisely when a `.` follows, because the VM
   resolves that exact name to `self` as the `$.foo` invocant carrier. So
   `target_var` already arrives as `Some("__ANON_STATE__")` for `$.x`, and as
   `Some("self")` for `self.x`. No new argument was needed.
2. "Silent over-mutation is gone." **False.** `$.x++` still mutated the
   attribute (`has $.x = 5`, `$.x++; $.x++` left it at 7), and the file's own
   table never probed an increment. That was the last silent data loss in the
   family and is the reason this turned out worth doing now.

## What is still open

`$.attr .= meth` on a non-`rw` scalar accessor. Measured:

```raku
class F { has $.s = "a";
    method q1 { $.s = $.s.uc; "attr=$!s" }   # raku: dies. mutsu: dies. agrees.
    method q2 { $.s .= uc;    "attr=$!s" }   # raku: attr=a. mutsu: dies.
}
```

`.=` is a read-modify-write, so raku sends it to the throwaway like every other
RMW. mutsu refuses with `Cannot modify an immutable Str (a)`.

## Why it needs a design call rather than a one-line guard

The runtime decision is already implemented and is one boolean away — the
`AssignExpr` opcode carries `dot_twigil_rmw`, and `check_dot_twigil_accessor_writable`
answers "skip the store" for it. What is missing is a **carrier for the `.=`
origin through the parser**, and the two rows above prove no AST shape can
substitute for it: `$.s .= uc` and `$.s = $.s.uc` lower to the *same*
`AssignExpr { name: ".s", expr: MethodCall { target: Var(".s"), name: "uc" } }`,
and raku answers them differently.

Three routes, none free:

- **Mark it at the `.=` lowering sites.** `grep '"\.="' src/parser/` finds 11;
  the ones that can carry a `$.attr` target are `stmt/assign/assign_stmt.rs`,
  `stmt/simple_expr_stmt/core.rs` (the `Expr::Var(name)` arm), and
  `stmt/assign/try_assign.rs`, plus possibly the hyper form in
  `expr/postfix/loop_.rs`. Cheap per site, but the marker has nowhere to live
  (see the next route) and the set is easy to under-enumerate — that is exactly
  how `.=` was missed in the first place.
- **Add a field to `Expr::AssignExpr`.** The honest home for "this assignment is
  an RMW", and it would also let the `CompoundAssign` compiler flag retire. But
  there are 181 `AssignExpr {` construction sites; a struct-field add touches
  every one.
- **Reuse `Expr::CompoundAssign` as the annotation.** It already exists for
  exactly this purpose ("source-preserving annotation"), and the compiler's arm
  already sets the flag. But its `op` comes from the closed `CompoundAssignOp`
  enum (no `.=` member) and its `rhs` is walked *separately* from `expanded` by
  `collect_ph_expr` / `collect_ph_expr_shallow` in `ast.rs` and by
  `whatever_curry/mark/expr.rs`, so feeding it the method-call expression would
  double-count placeholders and `*`-curries, while feeding it `Nil` would make
  the annotation a lie.

## Also deliberately left out

`self.x = 9` and `self.x *= 2` report `X::Assignment::RO: method 'x' is not rw`;
raku uses the immutable-value wording (`Cannot modify an immutable Int (5)`) for
those too. The old file called this "cheap to align" and asked for it in the same
change so the two would not disagree about which message a `$.` write gets — but
after the RMW fix a `$.` write produces no message at all, and the simple
`$.x = v` already reports raku's wording, so nothing disagrees. Aligning it means
four separate `is not rw` sites in `methods_mut_method_lvalue.rs` (lines 773,
851, 954, ~1084) plus `methods_mut_dispatch.rs`, each of which must render the
*current* attribute value, and it changes a message several `t/` files' prose
refers to. Worth its own slice; not urgent, since it is cosmetic.

## Affected files

- `src/parser/stmt/assign/assign_stmt.rs`, `src/parser/stmt/simple_expr_stmt/core.rs`,
  `src/parser/stmt/assign/try_assign.rs` — the `.=` lowering sites
- `src/ast.rs` — `Expr::AssignExpr` / `Expr::CompoundAssign`, and the two
  placeholder walkers that constrain the annotation route
- `src/compiler/expr.rs`, `src/compiler/expr_data.rs` — the existing
  `dot_twigil_rmw_assign` flag, ready for whatever carrier is chosen
- `src/vm/vm_misc_assign.rs` — `check_dot_twigil_accessor_writable`, which
  already answers the question
