use Test;

# Found via IRC::Log::Textual: IterationBuffer is an Any, so Any's list-shaped
# methods run over its elements.
plan 9;

my $b := IterationBuffer.CREATE;
$b.push($_) for 1..5;

is-deeply $b.head, 1, 'head';
is-deeply $b.head(2).List, (1, 2), 'head(2)';
is-deeply $b.tail(2).List, (4, 5), 'tail(2)';
is-deeply $b.first(* > 2), 3, 'first(&matcher)';
is-deeply $b.skip(2).List, (3, 4, 5), 'skip(2)';
is-deeply $b.grep(* > 2).List, (3, 4, 5), 'grep';
is-deeply $b.map(* * 2).List, (2, 4, 6, 8, 10), 'map';
is-deeply $b.min, 1, 'min';
is-deeply $b.max, 5, 'max';

done-testing;
