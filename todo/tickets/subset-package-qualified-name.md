# A `subset` declared inside a package is not package-qualified

A `subset` declared in the body of a `class`/`module`/`role` belongs to that
package, so raku names it `Package::Name`. mutsu registers it under its short
name only, so `.^name` and every error message that prints the constraint report
the unqualified name.

## Repro

```raku
unit class Foo;
subset RM of Str where any(<GET POST>);
has RM $.method is rw;
method set($m) { $!method = $m }
say RM.^name;
try { Foo.new.set('X') };
say $!.message;
```

```
raku:   Foo::RM
        Type check failed in assignment to $!method; expected Foo::RM but got Str ("X")
mutsu:  RM
        Type check failed in assignment to $!method; expected RM but got Str ("X")
```

## Root cause

`subset` registration does not consult the current package the way `class`/`role`
declarations do (the `SetCurrentPackage` machinery added for PLAN 8.22, `unit
module` package scoping). The declared name is stored verbatim, so both the
metamodel name and the `type_constraint` string recorded on the attribute stay
short.

## Affected files

- the `subset` registration path (`src/runtime/registration_*`, the `SubsetDecl`
  handling) — where the qualified name must be composed
- `src/runtime/types/` — resolution must still accept the SHORT name from inside
  the declaring package (and from a `where` clause in the same body), so this is
  a two-sided change: register qualified, resolve both
- the attribute `type_constraint` string recorded by `HasDecl`, which is what
  `X::TypeCheck::Assignment` prints

## Why it is not just a rename

The short name has to keep resolving lexically inside the declaring package, and
an exported subset has to be reachable by both names from an importer. The same
"register under the qualified name, resolve short-first" shape that PLAN 8.22 put
in place for `unit module` scoping is the model to follow.

## Impact

`HTTP::UserAgent`'s upstream `t/040-request.rakutest` subtest 18 ("rejects wrong
method") — the last failure in that suite, which is otherwise 26/27. The
exception is now correctly thrown (PLAN 8.21, fixed 2026-07-25); only the type
NAME in its message differs, and the test matches on
`/'expected HTTP::Request::RequestMethod but got Str'/`.
