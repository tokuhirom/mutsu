# A closure referencing an inner sub as `&t` captures its free variables

mutsu#9106 taught the compiler that a closure calling a routine-nested named
sub (`sub e($p) { my sub t() { $p * 2 }; -> { t() } }`) has to capture the
sub's free variables itself, because such a sub resolves them against the env
live at call time. The fold only ran at *call* sites, so a closure that merely
referenced the sub as a code variable was left out:

```raku
sub e4($p) { my sub t() { $p * 2 }; -> { &t } }
say e4(7)()();   # rakudo 14, mutsu printed 0
sub e5($p) { my sub t() { $p * 2 }; -> { &t() } }
say e5(7)();     # rakudo 14, mutsu printed 0
```

Both the plain `&t` read (`Expr::CodeVar`) and the `&t(...)` call-on form now
fold the sub's `lexical_sub_free_vars` entry into the closure's capture set,
through `Compiler::fold_lexical_sub_free_vars_for_code_var`. A `&t` local of the
closure itself (a `&t` parameter or `my &t`) shadows the inner sub and folds
nothing. Pinned in `t/routines/closure/closure-calls-routine-nested-sub.t`
(mutsu#9110).
