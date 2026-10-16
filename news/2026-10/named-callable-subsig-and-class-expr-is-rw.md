# `:&run!` in a sub-signature and `is rw` on a class expression

Two gaps that stopped Red's test suite early:

- A `:&name` parameter in a sub-signature (`with %h<a> -> % (:$times!, :&run!) { ... }`) looked the value up under the key `&name` and so always died with "Required named argument '&run' not passed"; the `&` sigil is now stripped like `@` and `%` are.
- `my $x = class D is rw { ... }` treated `rw` as a parent class ("cannot inherit from 'rw'"); in expression position `is rw` is now the class trait, as in statement position.
