# A `sub EXPORT` module no longer leaks its own operator imports

To know what a module with a run-time `sub EXPORT` hook hands its importer,
mutsu's parser takes a superset: everything the module itself imported,
because a hook may re-export another module's stash. That superset also
carried the operator candidates the module imported only for itself — Red's
`infix:<eq>` for its AST nodes, through `Red::Migration::Configuration` — so
the importer parsed every `eq` as a call of a user operator it could not see,
and an operator that only the module imported parsed at all instead of being
a compile-time error.

The superset now leaves out `infix`/`prefix`/`postfix` candidates unless the
module names a stash it could hand on (`Other::EXPORT::DEFAULT`, `UNIT::`,
`MY::` and the like); routines and terms a hook builds its `Map` from keep
coming through, so Red's `config { ... }` files still parse (#18496).
