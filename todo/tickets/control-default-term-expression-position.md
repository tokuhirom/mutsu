# `default { }` cannot be used as a term nested inside an expression

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
854).

## Repro

```
given 42 {
    "a".say;
    $_ == 42 and ( default { "b".say; 43 } );
    "c".say;
}
```

- raku: prints `a` then `b` (the `and`'s short-circuit means `"c".say` is never reached because
  the `default` block, once entered, exits the enclosing `given`)
- mutsu: `===SORRY!=== Error while compiling ... Unexpected block in infix position ...`

## Root cause guess

`default { }` is presumably only recognized by the parser/compiler in statement position
directly inside a `given`/`when` chain, not as a term that can appear nested inside another
expression (here, as the RHS operand of `and`, itself inside parens).

## Affected files (starting point)

- `src/parser/` — wherever `default`/`when` blocks are parsed as statements vs. terms
- `src/compiler/stmt.rs` / `src/compiler/expr.rs` — given/when/default compilation

## Suggested next step

Check how `when { }` is parsed when the grammar allows statement-modifier and nested forms
(e.g. `raku-doc/doc/Language/control.rakudoc`'s other `when`-as-expression examples) and see if
`default` can share that same term-level parse path.
