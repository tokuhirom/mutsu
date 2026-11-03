# `t/oo/forward-declaration.t` uses rakudo's forward-declaration spelling and joins the EVAL($ast) ratchet

The last file of #15909 that failed through `EVAL(slurp($f).AST)` used `sub add($a, $b);` and
`sub sum4($$$$)`, which rakudo rejects at parse time, so the file could not run as an AST on any
implementation. It now spells the forward declarations `{...}` with a `($, $, $, $)` signature
(identical under `raku`, mutsu and the AST driver) and is listed in
`ci/rakuast-ast-eval-passing.txt`. The nested-scope routine shadowing the issue describes was
landed earlier by the per-scope redeclaration check (`eval_scope_redecl.rs`). Closes #15909.
