# `(EXPR).method` is exactly `EXPR.method` — a parenthesized scalar keeps its container

The doc-diff harness on `Language/variables.rakudoc:134` reported that

```raku
my ($g) = 7, 8, 9;
say ( ($g) ).VAR.^name;   # raku: Scalar    mutsu: Int
```

and the ticket (`todo/tickets/paren-single-var-decl-var-scalar-name.md`) blamed
the parenthesized-single-variable declaration form, guessing that
`my ($g) = LIST` stored the extracted value without wrapping it in a `Scalar`
container.

## The ticket's root cause was wrong

`my ($g) = LIST` was never the problem. Probed directly:

```
my ($g) = 7,8,9;   say $g.VAR.^name;        # mutsu: Scalar   (correct)
my $j   = 1;       say ($j).VAR.^name;      # mutsu: Int      (wrong)
```

The declaration was fine; **the parentheses in the probe** were what lost the
container. Any `($x).VAR` — one layer or two, with or without inner whitespace
— reported the contained value's type. `(@a[0]).VAR` happened to still work,
which is why the divergence looked declaration-specific.

## Real root cause

`Compiler::compile_expr`'s `Expr::MethodCall` arms dispatch on the *shape* of
the target: a `.VAR` on `Expr::Var` takes the container-reflection path (and
registers the decl-site boxing ADR-0057 needs), a `.VAR` on `Expr::Index` takes
the element-metadata path, and so on. None of those arms matched
`Expr::Grouped(Var(_))`, so a parenthesized target fell through to the generic
value-level method dispatch, which sees only the decontainerized value.

In Raku, parentheses are pure grouping: they never introduce a container and
never strip one, so `(EXPR).method` must be compiled as `EXPR.method`. The fix
peels the `Grouped` wrapper off a method-call target once, at the top of the
`MethodCall` arms, rather than teaching each specialised arm about it — the
peeled call re-enters `compile_expr`, so nested parens unwrap for free.

Pinned by `t/parser-expression-gaps.t`.

## A second divergence the peel exposed

Peeling the group routes `(EXPR).method` through the same *mutating-variable*
dispatch a bare `EXPR.method` takes (`CallMethodMut` rather than `CallMethod`),
and the two turned out to disagree on one method: `.list` on an `is Array`
subclass instance. The non-mut path delegates any non-user method to the backing
`__mutsu_array_storage` unconditionally, while the mut path consults a
hand-maintained list of delegated method names — and `"list"` was missing from
it next to `"List"`, `"Array"`, `"Seq"`, `"Slip"` and `"flat"`. So `$v.list`
returned `([1, 2, 3],)` (the instance wrapped in a one-element list) where raku
returns the elements, and `@$v` — which lowers to `($v).list` — only worked by
taking the other path.

`"list"` is added to that delegation list, so both opcodes now agree and
`$v.list` matches raku. Pinned by a new assertion in
`t/array-subclass-vector.t`.
