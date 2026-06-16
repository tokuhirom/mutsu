use Test;

plan 9;

# .reverse on a Seq (the result of .comb, .map, .grep, etc.) must work,
# not throw "No such method 'reverse' for invocant of type 'Seq'".
is "abc".comb.reverse.join, "cba", '.comb.reverse.join';
is-deeply "abc".comb.reverse.List, ('c', 'b', 'a'), '.comb.reverse elements';

is-deeply (1..3).map(* + 1).reverse.List, (4, 3, 2), '.map(...).reverse';
is-deeply (1..5).grep(* > 2).reverse.List, (5, 4, 3), '.grep(...).reverse';

# Explicit Seq / Slip.
my $s = (3, 1, 2).Seq;
is-deeply $s.reverse.List, (2, 1, 3), 'Seq.reverse';
is-deeply <a b c>.Slip.reverse.List, ('c', 'b', 'a'), 'Slip.reverse';

# An empty Seq reverses to an empty list.
is-deeply ().Seq.reverse.List, (), 'empty Seq.reverse';

# Reversing a Seq twice round-trips.
is-deeply (1, 2, 3, 4).Seq.reverse.reverse.List, (1, 2, 3, 4), 'Seq.reverse.reverse round-trips';

# .reverse on a sorted Seq.
is-deeply (3, 1, 2).Seq.sort.reverse.List, (3, 2, 1), 'Seq.sort.reverse';
