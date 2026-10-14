# `Hash.new` on an `is Hash` subclass stores every pair as an entry

`class E is Hash { has $.a }; E.new(a => 3)` initialized the attribute `$.a` and
left the pair out of the hash. Rakudo's `Hash.new(*%)` never calls `bless`: every
pair becomes an entry and a declared attribute stays uninitialized (its default
does not run either). The subclass constructor now does the same, so `E.new(a => 3)`
has the key `a` and `E.new(a => 3).a` is `Any`. Closes #13182.
