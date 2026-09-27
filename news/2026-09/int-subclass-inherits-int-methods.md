# An `Int` subclass inherits `Int`'s methods and its `++` / `--`

`class MyInt is Int {}; my $x = MyInt.new(5); $x++` left `$x` at `1`: the
instance keeps its integer in the reserved `__mutsu_int_value` attribute, which
value-level coercion (`+`, stringification) already read, but the native method
layer did not. `.succ` / `.pred` / `.abs` / `.sqrt` died with "No such method",
`.is-prime` answered `False`, `.Int` / `.Numeric` hit an internal "fallback
disabled" error, and `++` fell back to its seed of 1.

The native 0- and 1-argument method entry points now answer `Int`'s methods on
the payload for such an instance (`src/builtins/int_subclass.rs`), keeping the
methods about the instance itself (`WHAT`, `WHICH`, `raku`, `gist`, `clone`, …)
on the instance. `.Int` / `.Numeric` / `.Real` return the invocant, as in
Rakudo. `++` / `--` take `Int`'s own candidate the way Rakudo's multi dispatch
does: the payload steps to an `Int`, and a user `.succ` / `.pred` declared on the
subclass answers explicit calls but is not consulted by `++` / `--`.

Two paths that take their numbers from the argument rather than through method
dispatch read the payload too: `sprintf` / `.fmt` (`%d` formatted such an
instance as `0`) and `.polymod` (which answered `(0 0)`).

Regression test: `t/types/numeric/int-subclass-int-methods.t`. Fixes #9906.
