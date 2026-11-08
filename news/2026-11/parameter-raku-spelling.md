# `Parameter.raku` spells slurpy hashes, captures, sigilless and renamed parameters

`*%o` rendered as `*:%o`, `|c` as `c is raw`, an anonymous `|` as an empty
string, a sigilless `\x` as `x is raw` and `:z($w)` as `:$w`. They now render
as declared, as Rakudo does. A `where` clause on a capture is still not
spelled.
