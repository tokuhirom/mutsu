# RakuAST no longer renders mutsu's internal desugaring markers

`.AST` emitted nodes that cannot exist in a real RakuAST tree. mutsu desugars a
handful of constructs in the parser into calls to internal routines, or into
temporaries with internal names, and the converter rendered those names
verbatim:

```
$ mutsu -e 'my @a; say Q[-<<@a].AST'
...
    expression => RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier("__mutsu_hyper_prefix"),
```

raku keeps every one of these constructs as a dedicated node
(`-<<@a` is a hyper *prefix*, not a call) and has no such name anywhere, so the
rendered tree was not merely incomplete — it was wrong, and it would not survive
comparison with the reference implementation.

## Change

`src/rakuast/convert.rs` refuses a routine or variable name that is one of
mutsu's internal markers (`__mutsu_*`, `__with_tmp_N`, `@__destructure_tmp__`,
…) and reports it as a coverage boundary:

```
RakuAST: `.AST` does not yet support this construct: desugared construct (internal name `__mutsu_hyper_prefix`)
```

This is the rule the rest of the converter already follows, and the one
`docs/rakuast/README.md` states outright: *if the parser/internal AST has
already erased a distinction, do not guess it inside RakuAST conversion.* An
erased distinction is a boundary, never a guess. Nothing here asserts what the
right node would be — that needs measuring against raku first, and the
underlying constructs (hyper prefix, `with`/`without`, list assignment, …) stay
on the read-direction gap list in `todo/deep/rakuast-remaining.md`.

Only names that mutsu itself plants are affected. Ordinary calls, hyper method
calls, and variable declarations render exactly as before, and the whole
`t/rakuast*.t` suite passes unchanged — no previously-correct rendering was
lost, because these names never had a correct rendering.

## Coverage

`t/rakuast-desugar-boundary.t` (6 assertions) pins three constructs as
boundaries (hyper prefix, zip assignment, `with`) and three neighbouring
constructs as still rendering. It is deliberately mutsu-only: it asserts
mutsu's boundary behaviour, which has no raku counterpart, so unlike the rest
of the `t/rakuast*.t` suite it is not a dual-oracle file.
