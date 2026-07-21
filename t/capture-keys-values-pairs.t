use v6;
use Test;

# A Capture's introspection methods interleave the positional part (indexed
# 0..n) with the named part (raku Capture semantics), rather than exposing only
# the named arguments.

my $c = \(2, 3, 5, apples => (red => 2));

is $c.keys.raku,   '(0, 1, 2, "apples").Seq', '.keys = positional indices then named keys';
is $c.values.raku, '(2, 3, 5, :red(2)).Seq',  '.values = positional then named values';
is $c.kv.gist,     '(0 2 1 3 2 5 apples red => 2)', '.kv interleaves positional and named';
is $c.pairs.gist,  '(0 => 2 1 => 3 2 => 5 apples => red => 2)', '.pairs = index/named pairs';
is $c.antipairs.gist, '(2 => 0 3 => 1 5 => 2 (red => 2) => apples)',
    '.antipairs swaps and parenthesizes a Pair-valued key';

is \(1, 2, 3, apples => 2).Numeric, 3, '.Numeric = number of positional elements';
is \(1, 2, 3, apples => 2).elems,   3, '.elems = number of positional elements';

# A Pair-valued key is parenthesized in the gist so the outer arrow is clear.
is ((red => 2) => "apples").gist, '(red => 2) => apples', 'Pair-as-key gist is parenthesized';
is ("a" => 1).gist, 'a => 1', 'plain Str key is not parenthesized';

# `|$capture` flattens the capture as a no-paren listop argument, even when a
# trailing named argument follows.
my $x = \(4, 2, 3, -2);
is (unique |$x, as => {.abs}).gist, '(4 2 3)', 'unique |capture with trailing named arg';
my $y = \(1, 7, 3, by => {1/$_});
is (min |$y), 7, 'min |capture with named arg';
is (max |$y), 1, 'max |capture with named arg';

# A spaced `|` between two terms remains the any-junction infix.
is (1 | 2).WHAT.^name, 'Junction', 'spaced | stays a junction infix';

done-testing;
