# An undeclared package-qualified `@`/`%` slot is a Scalar, as in rakudo

`@GLOBAL::u = 1, 2; say @GLOBAL::u.raku` printed `[1, 2]`. Rakudo prints
`$(1, 2)` (#11000). A package-qualified variable that no `our` declared is
auto-created as an empty **Scalar**, so `=` item-assigns the right-hand side
into it rather than list-assigning into a fresh Array. A single Pair is stored
as `:a(1)`, a Range as `1..3`, and an Array or Hash as itself, itemized and not
copied. An element write vivifies an itemized container: `@GLOBAL::x[0] = 1`
is `$[1]`.

`SetGlobal` now checks for an undeclared package `@`/`%` slot and stores the
right-hand side itemized. A slot counts as declared when it holds a plain,
non-itemized Array or Hash, and a declared `our @a` keeps the ordinary list
assignment. The element-write prologue and epilogue from #10901 now cover
package arrays as well as hashes, and they moved to their own module,
`src/vm/vm_package_containers.rs`. An array element write inside a routine
therefore persists too: `sub s { @GLOBAL::y[1] = 5 }` leaves `$[Any, 5]`.
