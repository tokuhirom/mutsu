# `CORE::v6c` packages, `CORE::` type listing, `CORE::v6c::Name` constraints

Under `use v6.*`, `CORE::{"v6c"}` (also `v6d`, `v6e`) is the package of that revision's core types and
`.WHO{$name}` finds a core type by name; `CORE::` lists the core types. A parameter typed `CORE::v6c::Name`
(or the pinned `CORE::Name`) matches the plain core type, also as a defaulted parameter of a `multi`
candidate (the unset parameter used to bind a type object named after the spelling, so the candidate never
applied). `Bool.HOW` is an `EnumHOW`. Rakudo-Type-Introspection `t/01-basic` now passes (34/34).
