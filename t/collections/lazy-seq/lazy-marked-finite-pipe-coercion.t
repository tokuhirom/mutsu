use Test;

plan 11;

# `.List`/`.Array`/`.list`/`.values`/`.cache` keep a `.lazy` Seq lazy in Rakudo
# whether or not its source is finite (#10976). A map/grep pipe over an
# explicitly `.lazy` finite list carries that marker, so these coercions must
# not reify it; a plain gather pipe is not `.is-lazy` and still reifies.

is (1..3).lazy.map(* + 1).List.gist, '(...)', '.List of a lazy finite map stays lazy';
is (1..3).lazy.grep(* > 1).Array.gist, '[...]', '.Array of a lazy finite grep stays lazy';
ok (1..3).lazy.map(* + 1).List.is-lazy, '.List keeps .is-lazy';
ok (1..3).lazy.map(* + 1).list.is-lazy, '.list keeps .is-lazy';
ok (1..3).lazy.map(* + 1).values.is-lazy, '.values keeps .is-lazy';
ok (1..3).lazy.map(* + 1).cache.is-lazy, '.cache keeps .is-lazy';
is (1..3).lazy.List.gist, '(...)', 'control: .List of a plain .lazy list';

# Iterating the lazy result still yields every element.
my @seen;
for (1..3).lazy.grep(* > 1).List { @seen.push($_) }
is @seen, [2, 3], 'the lazy .List still iterates all elements';
is (1..3).lazy.map(* + 1).List.eager, (2, 3, 4), '.eager reifies it';

# A plain gather pipe is not lazy and reifies.
is (gather { take 1; take 2 }).grep(* > 0).List.gist, '(1 2)',
    'a gather pipe still reifies on .List';
nok (gather { take 1; take 2 }).grep(* > 0).List.is-lazy,
    'and is not lazy';
