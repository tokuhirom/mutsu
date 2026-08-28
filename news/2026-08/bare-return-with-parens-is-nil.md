# `return()` is a zero-argument call, so it returns `Nil`

`return()` returned an empty list where rakudo returns `Nil`. The distinction is
one of Raku's oldest whitespace rules and mutsu's parser dropped it on the floor:

```raku
sub a { return   };  say a().raku;   # Nil   -- both agree
sub b { return() };  say b().raku;   # mutsu: ()    rakudo: Nil
sub c { return ()  };  say c().raku;   # ()    -- both agree
```

An argument list attached to a name with **no** intervening space is that
routine's argument list, so `return()` passes *zero* arguments and is exactly a
bare `return`. Put a space in and the `()` stops being an argument list and
becomes a *term* — the empty list — passed as the one argument, so `return ()`
really does return `()`. `return(5)`, `return(1, 2)` and `return (5)` were all
already right; only the empty case was wrong.

`return_stmt` (`src/parser/stmt/simple/control_stmts.rs`) called `ws` immediately
after the keyword and only then parsed a value expression, which erased the
distinction before anything could act on it — `return()` and `return ()` produced
a byte-identical `Return(ArrayLiteral([]))` AST. The empty-attached-parens case
is now recognised before that `ws`, and takes the same `Expr::Literal(Value::NIL)`
path a bare `return` takes, including through a statement modifier
(`return() if 1`).

## What it freed

`roast/S04-statements/return.t` passes under both providers. It regressed under
`MUTSU_REAL_TEST=1` on tests 2 and 5 — `is(bar2(), Nil, ...)` for `sub bar2 {
return() }` and `sub foobar2 { return() if 1 }`. Measured on a rebuilt pre-fix
binary, mutsu's native `is` accepted the wrong value (`ok 1`) where the real
module's rejected it; rakudo's `is` dispatches a `Mu:U $expected` candidate that
compares with `===`, which `()` does not satisfy.

That is the general shape of this whole campaign's residue: the answer was
already wrong under the native provider too, and only a `Test` implementation
strict enough to ask the right question made it visible.

Pin: `t/bare-return-with-parens.t`, 17 assertions covering the bare form, the
attached-parens form (plain, with inner whitespace, before a newline, and under a
statement modifier), the spaced form, every non-empty argument shape, and the
same in a method and an anonymous sub — green under real `raku` unchanged.
