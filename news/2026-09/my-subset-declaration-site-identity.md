# `my subset` gets declaration-site identity

A lexical subset used to be registered under its bare name, so two same-named
`my subset` declarations in different scopes shared one registry entry and
the last declaration won everywhere. `Java::Generate` shows the failure:
`PrefixOp`, `PostfixOp` and `InfixOp` each declare their own
`my subset Op of Str where %known-ops{$_}:exists`. Only `InfixOp`'s survived,
so `PostfixOp.new(:op<++>)` died with
`Type check failed in assignment to $!op; expected Op but got Str ("++")`.

A `my subset` now gets the same ADR-0047 treatment `my class` already had. It
is stored under `Name\0<declaration-site id>`, and its bare name is bound to
that storage name in the declaring scope. The storage name is package-qualified
the way Rakudo's `fully_qualified_with($package)` names a subset, so `.^name`
is `Holder::Small` for a `my subset Small` declared in `class Holder`. The owner
walk that resolves attribute types finds the class's own subset. A type object
that escapes its block keeps its own predicate. The mangled key cannot be
spelled, so `module M { my subset F ... }` is still not reachable as `M::F`.
`Code.of` answers the type object that the return constraint's spelling is
bound to, so `(--> ofTest).of =:= ofTest` still holds.

With this change, `Java::Generate` goes from 5/9 to 9/9 baseline files at
parity. The `my role` half of #9894 is still open.
