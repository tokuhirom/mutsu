# An untyped variable is not held off its fast paths by a typed one elsewhere

`slot_type_constraint_possible` answered "could a type constraint exist for this slot?" from a
per-name latch raised whenever any variable of that name was ever declared typed. One
`my Str $s` in a loaded module therefore took every untyped `$s ~= ...`, `$s++` and
`my $s = ...` of the program off its fast path, and `$s ~= 'a'` in a loop after
`use Cro::HTTP2::RequestParser` cost 7x the bare program. The slot's own declaration already
settles it (`DeclaredConstraint::Untyped`, which the plain scalar store acted on since
ADR-13757); the latch is now asked only for a slot whose declaration does not.

Callgrind slope (profiling build, warm, `$s ~= 'a'` loop, 50000 -> 450000 iterations, with
`use Cro::HTTP2::RequestParser`): 8.6K -> 2.5K Ir per iteration (bare: 1.24K).

Test: `t/types/untyped-slot-ignores-same-named-typed-variable-elsewhere.t`.
