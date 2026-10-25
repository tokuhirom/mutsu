# A term-form class keeps the HOW mixin its trait made, and accepts `is trait(arg)`

`my \c = class C is trait { }` and `given class ... { }` declare the class twice: a compile-time shell
that runs the `trait_mod:<is>` traits once (#12708), and the in-place registration, which does not
run them again. The in-place registration still dropped any role the shell's traits had mixed into
the class's HOW (`$class.HOW does R`) as a stale leftover of a hoisted pre-pass, so `C.HOW` fell back
to a bare `ClassHOW` and `C.^method-of-R` failed. It now keeps the mixin when the shell already
dispatched the traits. Red's `model C is rs-class(MyResultSet) {}` inside `lives-ok { ... }` is the
case this fixes (`t/01-tdd`).

The term form's header parser also accepted only `is Parent`: `class B is mp(Int) { }` died with
"Two terms in a row". A lowercase `is trait(expr)` / `is trait<word>` is now handed to
`trait_mod:<is>` with its argument, as the statement form does.
