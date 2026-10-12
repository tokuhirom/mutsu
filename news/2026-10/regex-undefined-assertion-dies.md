# `<$var>` holding an undefined value dies, as in rakudo

`/<$n> a/` with `$n` unset (an `Any`, a typed `Str:U`, or an in-regex `:my $n`
left unassigned) used to fail the match without a word. Rakudo dies here: its
`INTERPOLATE_ASSERTION` has no candidate for an undefined argument. mutsu now raises
the same `X::Multi::NoMatch`, with rakudo's message, both from the parse-time
`<$var>` path and from the match-time path that reads an in-regex `:my` (#13042).
