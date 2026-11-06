# `my ::?CLASS:U $x` declares a class-typed variable, not an unbound capture

A declaration `my ::?CLASS:U $class = ...` was treated like `my ::T $x` (an unbound
type capture, typed `NQPMu`), so any assignment failed the type check. This is what
`Font::AFM.core-font` does, which kept `FontFactory::Type1` t/04-font-afm.t at 0/5.
`::?CLASS` now constrains to the enclosing class when it is known at compile time and
keeps its `:U`/`:D` smiley; the class check is relaxed to `Mu` where the class is not
known to the compiler (a method body, a role body). FontFactory::Type1 now matches
`raku` on all 13 test files.
