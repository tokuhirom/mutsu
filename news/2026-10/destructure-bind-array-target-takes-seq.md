# `my ($k, @v) := ...` binds a Seq to the `@` target

`my ($str, @bind) := do given $pair { .key, .value }` died with "Type check failed in
binding; expected Positional but got Seq" whenever the Pair's value was a `Seq` -- Red's
`translate` for `Red::AST::Value` builds exactly that (`'(...)' => .get-value.map: {...}`),
so every `IN (...)` filter failed. A signature bind hands an `@` target a Seq through its
cached List, the way an `@` parameter does; a lone `my @a := SEQ` is still refused, and a
`$` target still keeps the Seq itself.

Refs #14094.
