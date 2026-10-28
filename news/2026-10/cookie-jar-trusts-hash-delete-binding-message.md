# Cookie::Jar: lexical-class `trusts` in module files, `:delete` in loops, binding messages

Three gaps found working Cookie::Jar, whose five test files now match `raku`:

- `$o!Pkg::Item::meth` on a `my class` of a module file is trusted when `Pkg::Item` says `trusts Pkg`
  (the trust list is keyed by the mangled storage name; the call site resolved the plain one).
- `$h<missing>:delete` on a scalar-held Hash in a loop body no longer deletes from, and hands back,
  the previous iteration's Hash.
- A wrong-typed argument for `Str:D $k is copy` / `is raw` reports `expected Str but got Int`, no smiley.
