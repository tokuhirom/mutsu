# A class exported from a compunit under another namespace is importable, and typed attributes use their owner's imports

`class A::B::EmptyCI is export` declared in `A/B/CI.rakumod` (a name that is not
nested in the compunit's own module name) is now imported by its last part
(`EmptyCI`), as rakudo does; the export was filed under the full name and never
bound the short one. Separately, a typed attribute such as `has Thing $.t` in a
class whose compunit did `use Bar::Thing` resolved `Thing` against the alias
table of whichever package was running when the object was built, so
`Foo::Rep.new` from a program that never imported `Bar::Thing` died with a type
check failure; it now resolves in the owner's own chain. Found through
App::Racoco::Report::ReporterCoveralls (11 of 12 test files now match rakudo;
before, 9 died at load).
