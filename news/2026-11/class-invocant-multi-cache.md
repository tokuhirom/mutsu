# `::?CLASS` invocants keep multi methods in the resolution cache

A multi method whose candidates differ only by an invocant smiley such as
`multi method WHICH(::?CLASS:D:)` / `(::?CLASS:U:)` used to re-run candidate
selection on every call. The resolution cache refused it because the
cacheability check read `::?CLASS` as an unregistered qualified name. It
treats value-dependent constraints that way, since an enum value such as
`Order::Less` matches only one value. `::?CLASS` and `::?ROLE` name the
candidate's own class, or for a role method the class that composes the role.
That is a fixed nominal type for a given receiver class, and the receiver class
and the invocant's definedness are already part of the cache key.

Red declares `WHICH` this way on every `Red::AST` node. One `.WHICH` on an
`Eq` node fell from 1.363M to 1.254M instructions (#17552). A role-composed
`multi method WHICH` benchmark dropped 16%.
