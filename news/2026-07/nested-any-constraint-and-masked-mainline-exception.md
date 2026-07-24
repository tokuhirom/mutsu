# A `Foo::Any` constraint stops matching everything, and a mainline exception stops being swallowed

Two independent bugs, both found while triaging the bundled Zef battery's
`t/distribution-depends-parsing.rakutest`. The second is what made the first
hard to find, so it is worth reading in that order.

## A mainline exception under `Test` was replaced by the plan mismatch

A program that dies in its mainline while using `Test` reported nothing about
the exception:

```raku
use Test;
plan 3;
ok 1, 'first';
Any.no-such-method;
```

```
# was
1..3
ok 1 - first
Runtime error: Test failures
# You planned 3 test, but ran 1
```

`run()` handled a failed mainline with `self.finish()?; return Err(e)` — and
`finish()` runs the end-of-program TAP checks. Under `Test`, a mainline
exception always leaves the plan short, so `finish()` returned its own "Test
failures" plan-mismatch error, the `?` propagated *that*, and the original
exception was discarded. Every failure of this kind therefore looked like a plan
bug, with only `# You planned N test, but ran M` to go on.

`finish()`'s side effects — the diagnostics it writes and the exit code it sets —
are still wanted, so it is now called for effect and its substituted error
dropped. mutsu prints the exception and then the plan diagnostic, matching raku.
Pin: `t/mainline-exception-not-masked-by-plan.t`.

## A class nested as `Foo::Any` was equated with the core `Any`

```raku
class Spec { }
class Spec::Any { }
multi f(Spec::Any $s) { 'ANY' }
multi f($s)           { 'GENERIC' }
say f(Spec.new);   # raku: GENERIC   was: ANY
```

`Interpreter::type_matches` carries a "qualified name matching" bridge: when one
name is qualified and the other is bare, their trailing components are compared,
so a type declared under a `unit module` (registered as `GH2613::R1`) still
matches a bare `R1` reference inside that module.

Comparing only the trailing component equated `Spec::Any` with `Any`. That was
not merely a wrong match, it was catastrophic: every class's MRO ends in `Any`,
`Mu`, so walking the MRO reached the `constraint == "Any"` universal arm and the
constraint accepted *every* instance. Renaming the class to `Spec::Alt` made it
behave correctly, which is what isolated the trailing component rather than
multi-dispatch ranking as the cause.

The bridge now refuses core setting type names. A core type never lives under a
user package, so a nested `Foo::Any` is definitionally not `Any`; the
registration gap the bridge exists to close cannot involve one. Pin:
`t/nested-any-type-constraint.t`.

In Zef this is what sent a plain `Zef::Distribution::DependencySpecification`
into the `…::DependencySpecification::Any`-constrained `spec-matcher` candidate,
which calls `.specs` — a method only the `::Any` sibling has — and died with
`X::Method::NotFound`. `t/distribution-depends-parsing.rakutest` goes from 18/35
to 20/35; the next blocker there is an `any(...)` dependency spec with one
satisfiable alternative failing to resolve, recorded in PLAN.md §1 B1.
