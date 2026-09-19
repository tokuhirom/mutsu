# Class-scoped subs resolve `::?CLASS` before multi dispatch

`Math::Quaternion` exports operator multis whose parameters use `::?CLASS:D`.
mutsu left that pseudo-type unresolved for ordinary subs declared in a class,
so the candidates behaved as unconstrained values. Its `infix:<*>` then matched
Arrays used by the test plan, and its `infix:<eqv>` intercepted Array comparisons
inside `Test.rakumod`.

Class-scoped ordinary subs now resolve `::?CLASS` to their declaring class when
they are registered, matching the existing method-registration behavior. The
regression is pinned by `t/oo/class/class-scoped-sub-pseudo-type.t`.

Found via the ecosystem roulette on #7884. `Math::Quaternion` 0.2.1 moves from
partial (3/6 baseline files, 21/101 assertions) to green (6/6, 101/101).
