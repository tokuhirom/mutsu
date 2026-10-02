use Test;

# From the SQL::Builder distribution: `set({bar => 'baz'})` into `*@values`.
# A non-itemized Hash/Map argument flattens into its Pairs under a `*@`
# slurpy; an itemized one (`my $h`) stays a single element.

plan 5;

sub f(*@v) { @v }

is-deeply f({bar => 'baz'}).List, (:bar<baz>,), 'block-hash literal flattens to a Pair';
my %h = x => 2;
is-deeply f(%h).List, (:x(2),), '%-variable flattens to its Pairs';
my $s = {a => 1};
is f($s).elems, 1, 'a $-held Hash stays one element';
is f($s)[0].^name, 'Hash', 'and it is still a Hash';
is f([1, 2], [3]).elems, 3, 'Arrays still flatten one level';

done-testing;
