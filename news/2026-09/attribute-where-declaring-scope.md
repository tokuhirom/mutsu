# Attribute `where` clauses run in their declaring scope

An attribute's `where` block is re-checked after `BUILD`, during construction
driven from the caller's compunit. That check ran with the caller's unit and
package in effect, so a predicate calling a sub private to the class's own
module — `has Str $.locale is rw where { check-locale($_) }` beside a
`unit class` — died with "Unknown function", and the constraint read as failed:
`Type check failed in assignment to $!locale; where constraint failed` for
every valid value. The check now switches to the declaration's compunit and
package, the same scope an attribute default already evaluates in.

While there, an `is rw` accessor store (`$obj.x = -3`) now enforces the
attribute's `where` clause as well as its type, dying with rakudo's
`expected <anon> but got Int (-3)`; it previously accepted any value the type
allowed.

Found by the ecosystem `Date::Calendar::Gregorian` suite, whose
`t/03-accessors.rakutest` goes from dying before its first assertion to 27/27.
