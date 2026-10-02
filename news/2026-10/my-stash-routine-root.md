# `MY::` in a routine body no longer lists outer routines

A routine's or closure's own top-level `MY::` used to list every routine
visible by name — file-scope subs and everything an outer `use Test`
imported — so `sub r { MY::<&plan> }` returned `plan` where Rakudo answers
`Nil`. The top-level pad of a routine, method or closure body is now treated
like a nested block's (#10626): it holds the routines the body declares and
those its own `use`s import, and nothing else. A compunit's own `MY::` and
`OUTER::MY::` reaching it are unchanged, and `LEXICAL::` keeps listing every
visible routine.

Routines are now also recorded in their scope's pad when they are hoisted, so
a `MY::<&f>` read before `sub f`'s text finds it, as in Rakudo.
