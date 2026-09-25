# `[&TERM]` works as an infix for any `&`-term

[#9327](https://github.com/tokuhirom/mutsu/issues/9327), split out of the
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) parse-gap cluster.
Terminal::UI's `lib/Terminal/UI.rakumod:128` has

```raku
$current = ($current [&($op)] 1) % $count;
```

and mutsu rejected it with `===SORRY!=== Confused`, so Terminal::UI (and
App::samaki, which loads it) could not load.

The `[&name]` infix form was recognised only when the bracket held a plain
identifier: `parse_infix_func_op` scanned to the first `]` and required the
text to be `[A-Za-z0-9_-]+`. rakudo's `infixish` takes `[&` followed by any
`&`-term (a `&(...)` contextualizer, a qualified `&infix:<+>`, and so on) and
calls it with the two operands.

`precedence_meta_ops/infix_term.rs` now covers every other spelling. It parses
the term with the ordinary `code_var` parser, requires the closing `]`, and
lowers the operator to a call on the callable the term evaluates to:
`CallOn(term, [left, right])`, with the operands reversed for `R[&...]`, and
`cross` / `zip` with `:with(term)` for `X[&...]` / `Z[&...]`. A plain
`[&name]` keeps its existing `Expr::InfixFunc` path.

Pinned by `t/lang/operators/infix-bracket-code-term.t`.
