# `{ BLOCK } for LIST` (bare block as a `for` statement-modifier operand) is parsed as an uncalled closure term

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/py-nutshell.rakudoc:541`).

## Root cause hypothesis

`EXPR for LIST` (the `for` statement-modifier) evaluates `EXPR` once per element of
`LIST`, with `$_` bound to the current element. When `EXPR` is a bare `{ ... }` block,
real Raku treats the block the same way it would treat the body of `for LIST { ... }`
written the other way around: the block is invoked per iteration with `$_` set, and its
return value is collected.

mutsu's `--dump-ast` shows that `{ $_[0] + $_[1] } for LIST` compiles the block as an
`Expr::AnonSub` term inside the `for`'s body — i.e. each iteration evaluates the
`{...}` as a *closure literal* (producing a `Sub` value) rather than *calling* it with
`$_` bound. The loop therefore collects a list of never-invoked closures instead of
their results, and `say`ing that list produces empty output.

## Minimal repro

```raku
say ( { $_ + 1 } for 1,2,3 );
```

- `raku`: `(2 3 4)`
- `mutsu` (`target/debug/mutsu`): `(  )` (three empty/blank items — the uncalled closures)

Confirmed unrelated to the doc's `X`-cross or sigilless-parameter forms: the same
bare-block shape fails standalone, while the equivalent bare-expression form (no
braces, e.g. `$_ + 1 for 1,2,3`) and the pointy-block forms (`-> $i, $j { ... } for ...`)
both already work correctly.

## Affected files (starting point)

The `for` statement-modifier's parsing of its `EXPR` operand — needs to special-case a
literal `{ ... }` immediately followed by `for` (or more generally, recognize this
shape as a block-body-for-loop rather than a bare closure term), similar to the
existing `say {...}` hash-vs-block disambiguation elsewhere in the parser. Likely in
`src/parser/` around statement-modifier / `for` postfix parsing.
