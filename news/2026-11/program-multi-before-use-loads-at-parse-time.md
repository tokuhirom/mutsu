# A program's `multi` no longer delays the next `use` to after the parse

A `use` that follows a `multi` declaration at the top level of a program now
loads its module while the program is parsed, like every other `use`
(ADR-17558). Until now such a `use` waited for the BEGIN-time prologue, so the
rest of the parse only knew what a static scan of the module could tell, and
the module's load-time output came after any syntax error found later in the
program.

The candidate order stays rakudo's. A `multi` is installed when the parser
reaches it, so the program's candidate takes its order from the parse: one
declared before `use M` wins an equally narrow tie against M's candidate, one
declared after it loses it, and `&f.candidates` lists them in source order.

```raku
multi trait_mod:<is>(Routine $r, :$tagged!) { say "mine" }
use M;                    # also exports a trait_mod:<is> with :$tagged!
sub f() is tagged { }     # mine
```

Making this work exposed one import bug: the top-level import of a module
without a `unit module` declaration hid every candidate of the family that was
already visible, including the program's own, and only the old load order had
kept the program's candidates out of its way. `multi greet(Str $x) {}; use M;`
now dispatches over both the program's and M's `greet` in either order.
