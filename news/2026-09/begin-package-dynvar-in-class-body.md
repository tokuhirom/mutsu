# `$*PACKAGE` now resolves inside a class-body BEGIN phaser

`$*PACKAGE` was always `Nil` inside a `BEGIN { }` block in a `class`/`role` body, instead of
resolving to the package currently being compiled (#8790). rakudo binds it there so compile-time
code — a `BEGIN`/`CHECK` phaser, and more generally a custom `trait_mod` handler invoked while the
class body's attributes/methods are being processed — can introspect the package it is running
inside of, e.g.:

```raku
class Foo {
    BEGIN {
        say $*PACKAGE.^name; # Foo
    }
}
```

The class-body statement walk already tracks a `defining_class` for the duration of exactly this
compile-time phase (so a `has`-declaration executed by a compile-time `EVAL` attaches to the right
class). `$*PACKAGE` is now bound to that same package, as a dynamic-scope env entry, for the
duration of the phaser statement, and restored to its previous value afterward — so nested class
bodies (`class Outer { class Inner { BEGIN { ... } } }`) each see their own innermost package.

Outside a compile-time phase (a bare class-body statement, or a method body), `$*PACKAGE` stays
unbound, matching rakudo (which answers a `Failure` there, not `Nil` — a separate, much smaller
discrepancy not addressed by this change).

This unblocks the ecosystem `AttrX::Mooish` distribution's custom `trait_mod:<is>` handler, which
inspects `$*PACKAGE.HOW` at compile time.
