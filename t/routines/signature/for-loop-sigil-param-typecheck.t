use Test;

# A `for` loop's `@`/`%` parameter carries the implicit Positional /
# Associative check a routine's parameter does (#17866).

plan 10;

throws-like { for 1, 2 -> @q { } }, X::TypeCheck::Binding::Parameter,
    message => "Type check failed in binding to parameter '@q'; expected Positional but got Int (1)",
    'an @ parameter refuses an Int';
my @c = 1, 2;
throws-like { for @c <-> @q { } }, X::TypeCheck::Binding::Parameter,
    'an rw @ parameter refuses an Int element';
throws-like { for 1 -> %h { } }, X::TypeCheck::Binding::Parameter,
    message => "Type check failed in binding to parameter '%h'; expected Associative but got Int",
    'a % parameter refuses an Int';

my @seen;
for [1, 2], [3] -> @q { @seen.push: @q.elems }
is @seen, [2, 1], 'an Array element binds';
@seen = ();
for ((1, 2).Seq,) -> @q { @seen.push: @q.join }
is @seen, ['12'], 'a Seq binds through PositionalBindFailover';
@seen = ();
for (1..3,) -> @r { @seen.push: @r.elems }
is @seen, [3], 'a Range binds';
@seen = ();
for ((1, 2).map(* + 1),) -> @m { @seen.push: @m.join }
is @seen, ['23'], 'a map result binds';
@seen = ();
for ({ a => 1 },) -> %h { @seen.push: %h.keys.join }
is @seen, ['a'], 'a Hash element binds';
@seen = ();
for (set(1),) -> %s { @seen.push: %s.keys.join }
is @seen, ['1'], 'a Set binds to a % parameter';
@seen = ();
for ((a => 1),) -> %p { @seen.push: %p.key }
is @seen, ['a'], 'a Pair binds to a % parameter';
