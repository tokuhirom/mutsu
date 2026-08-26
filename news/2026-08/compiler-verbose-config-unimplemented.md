# `$*RAKU.compiler.verbose-config` reports mutsu's real configuration

`say $*RAKU.compiler.verbose-config` died with
`No such method 'verbose-config' for invocant of type 'Compiler'`.

## Establishing what the method actually is

The ticket's repro (`say $*RAKU.compiler.verbose-config.WHAT`) **dies in rakudo
too** — `Cannot look up attributes in a Hash+{<anon|4>} mixin type object` —
because rakudo returns a `Hash` with a gist-customizing role mixed in, and
`.WHAT` on that mixin trips over its own type object. So the ticket's spelling
could not be the spec. Consuming it as a `Hash`
(`my %h = $*RAKU.compiler.verbose-config`) is the usage that works, and that is
what this implements against.

Measured on rakudo v2026.06, the return value is a map of **section name** to a
map of **config key to string value** — five sections (`Raku`, `moar`,
`kernel`, `distro`, `repo`), with the flat `section::key=value` lines the docs
show coming from the mixed-in gist, not from the data shape.

## What mutsu reports, and what it deliberately does not

Rakudo's sections describe the MoarVM build that produced the running binary:
compiler flags, `3rdparty/` library paths, the build machine's install prefix.
mutsu has no such build database, and inventing MoarVM-shaped keys would be
worse than useless — a config map is read to find out what is true, so a
plausible-looking fabricated value is a lie with a long half-life. mutsu
therefore reports only facts it can actually stand behind, in rakudo's
structure so a consumer walks it the same way:

- `Raku` — implementation, version, language revision, auth.
- `mutsu` — the interpreter's own version, compiler id, and the target
  OS/arch/family this binary was built for.
- `kernel` and `distro` — projected from the very same instances that answer
  `$*KERNEL` and `$*DISTRO`, so the two views can never disagree.

The key *set* legitimately differs from rakudo's, because the underlying build
systems do. That is reported honestly rather than papered over.

New file: `src/runtime/native_methods/compiler_config.rs`. `Compiler` now
dispatches through `native_compiler`, which answers `verbose-config` itself and
delegates everything the `Raku`/`Perl` identity object also answers to
`native_perl`.

Pin: `t/eval-compunit-introspection.t` asserts the *shape* — a non-empty map,
with non-empty `Raku`/`kernel`/`distro` sections and a named implementation —
rather than any value tied to a particular build, so it passes verbatim under
`raku` as well as mutsu.
