# A program's `proto` no longer delays the next `use` either

A `use` after a top-level `proto` of a program now loads its module while the
program is parsed, as one after a `multi` already does. A `proto` ranks no
candidate, so the order of the program's declarations against the module's is
unaffected. An imported family extends the program's `proto`, as it does on
rakudo with an only-star proto:

```raku
proto greet(|) {*}
multi greet(Str $x) { say "mine" }
use M;                       # exports multi greet(Int)
say &greet.candidates.elems; # 2
```
