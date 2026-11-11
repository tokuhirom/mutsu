# A default-parametric role is materialized once, and lexical type keys are indexed

`Monad::Maybe.some(5)` calls a method on the role `Monad::Maybe[::T = Any]`
and then `.new` on its pun. Both re-bound the role's type-parameter defaults
through the full signature binder on every call, about 65k instructions each
time. The defaults are evaluated in the role's declaring scope, so their
binding depends only on what is declared; the materialized parametrization is
now remembered per role for the current registry write generation, as rakudo
puns such a role once.

`is_my_scoped_type_name` and `unique_lexical_type_key` answered "which
registered type keys carry this source name before a NUL" (`my class` keys
are stored as `Name\0<id>`) with a `starts_with` scan over every class, role,
enum and subset key. The registry's index of those source names, which
already said whether any such key exists, now maps each name to its keys, so
both lookups read the matching keys directly.

`Monad::Maybe.some(5)` went from about 753k to 653k instructions per call,
and Monad's `t/14_concurrent_types.t` now finishes (3.5 minutes on 4 cores,
same TAP as rakudo) instead of being killed at the sweep timeout; rakudo
still takes 8 seconds (#17331 stays open).
