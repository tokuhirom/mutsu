# App::Racoco::Report::ReporterCoveralls: qualified `is export` names and `multi method of`

Taking `App::Racoco::Report::ReporterCoveralls` (and its `App::Racoco` dependency) from
`blocked_load` to 11 of 11 baseline files passing under mutsu (`t/00-tiny-http` fails under rakudo
too) fixed several general gaps:

- `multi method of(...)` / `multi method returns(...)` was parsed as a sub named `method` carrying
  an `of`/`returns` trait ("Malformed trait").
- A role doing a sibling parametric role inside a `unit module` (`does Key[IO::Path]`) now resolves
  the role's base name; the `[...]` suffix was disabling the sibling lookup.
- `multi method m(::?CLASS:D: ...) {...}` stubs in a role no longer die when the role is declared;
  an unimplemented one raises `X::Role::Unimplemented::Multi` when a class composes the role.
- `unit class A::B::C is export;`, `class A::B::C is export` and `unit module A::B::C is export;`
  now publish the short name `C` to the importer, also for a second importer of an already-loaded
  module and for the importing module's own routines.
