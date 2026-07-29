# A `when` succeed still escapes an `if`/bare block that declares no lexical

Raku exits only the INNERMOST enclosing block on a matched `when` — even an
`if` branch: `given 5 { if True { when Int { } }; say "after" }` prints
"after" on Rakudo. mutsu now absorbs the succeed in `DoBlockExpr` and in
`exec_block_local_scope_op`, which covers `do { when ... }` and any branch/bare
block that declares a block-local `my` (the compiler only wraps those in
`BlockLocalScope`). A branch or bare block WITHOUT declarations compiles to a
plain jump, so the succeed still travels to the enclosing `given`/`with` and
terminates it early.

Repro (mutsu skips "after", raku prints it):

```raku
given 5 { if True { when Int { } }; say "after" }
given 5 { { when Int { } }; say "after" }
```

Fixing it means either wrapping every branch body that syntactically contains a
top-level `when`/`default` in a block op (compiler scan, cheap), or giving the
plain-jump path a succeed barrier. Low practical impact — the common idioms
(`when` directly in `given`/`for`, `do { when }`) now match raku — but it is a
visible semantic difference. Pin candidates live in
`t/when-succeed-innermost-block.t` once fixed.
