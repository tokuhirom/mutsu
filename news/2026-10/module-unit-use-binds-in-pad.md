# A module's own `use` binds its variables and types in the module's pad

A module's top-level `use` used to import the variables, constants and types
of the used module as env aliases, which the load took back out again when the
module finished loading. The module's routines then found them through a
separate import record. Now such a `use` declares them as pad bindings of the
module's mainline, as a block's `use` already did (ADR-8751, #14140): the
compiler emits `my $x := <the module's $x>` for each name the parse-time scan
of the used module lists, and when the load finishes the bindings move into the
module's compunit lexicals next to its own file-scope `my`s. The module's named
subs, methods and returned closures read the live container, whatever the
caller has under the same name.

A name the module declares itself at its top level, such as
`my constant Atom is export` next to a `use` that also exports an `Atom`,
keeps the run-time import, because in rakudo the module's own declaration
shadows the import. The body of a `unit class` counts as the top level.

The bindings depend on what the used module exports, so the module's cached
mainline compile (ADR-11756) now records that scan as one of its inputs. A
cached compile is served only while the used module still exports the same
names in the same way: changing `our $x is export` into a `sub EXPORT` hook
that returns `$x` recompiles the importer. The compile and its validation read
the used module's own scan, not whatever this process happened to parse
before, so a warm cache keeps hitting.

Fixed on the way: a module's own readonly binding (`my $x := 'v'` at its top
level) marked the loading program's same-named `my $x` readonly, so
`my $x = 'c'; use M; $x = 'd'` died with "Cannot assign to an immutable value".
