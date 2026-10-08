# RakuAST: `:delete` on a multi-dimensional subscript

`@a[0;1]:delete`, `%h{1;2}:delete(COND)`, `@a[0;1]:k:delete` and
`@a[0;1]:exists:delete` now cross the RakuAST boundary as the postcircumfix's
`colonpairs`, like the single-dimension adverbs. The by-name delete builtins
(`__mutsu_multidim_delete`, its `_assoc` form and the two `_dyn` forms) are
read back from and rebuilt by `ast::subscript_adverb`; the `use v6.e` /
associative choice of builtin is a parameter of `expand`. Eleven more `t/`
files pass under `MUTSU_RAKUAST=1`. Part of S10 of #7564.
