use Test;

# A Range subscript on a `does Positional` instance is a SLICE of `AT-POS` reads,
# one per index the range names: `$vec[1..2]` is `($vec[1], $vec[2])`. mutsu only
# knew how to slice an instance through a comma-list index (`$vec[0, 2]`), so a
# Range — or anything producing one, such as `^2` — answered Nil.
# From Math::Vector's `has @.components handles <AT-POS>`.

plan 10;

class Vec does Positional {
    has @.components handles <AT-POS>;
    multi method new(*@x) { self.bless(components => @x) }
}

my $v = Vec.new(1, 2, 3, 4);

is $v[1].raku, '2', 'a single index still reads one element';
is $v[1..2].raku, '(2, 3)', 'an inclusive Range slices through AT-POS';
is $v[1..^3].raku, '(2, 3)', 'an exclusive-end Range slices';
is $v[^2].raku, '(1, 2)', '^N slices';
is $v[0, 2].raku, '(1, 3)', 'the comma-list index is unchanged';
is $v[1..1].raku, '(2,)', 'a one-element Range is still a slice';

# An explicit AT-POS (no `handles`) behaves the same.
class Own does Positional {
    has @.c;
    method AT-POS($i) { @.c[$i] }
}
is Own.new(c => [10, 20, 30])[1..2].raku, '(20, 30)', 'an explicit AT-POS slices too';

# Associative instances are untouched.
class Assoc {
    method AT-KEY($k) { "k:$k" }
}
is Assoc.new<a>.raku, '"k:a"', 'an AT-KEY subscript is unaffected';

# Plain containers are unaffected.
my @a = 1, 2, 3;
is @a[1..2].raku, '(2, 3)', 'an Array Range slice is unchanged';
my %h = a => 1;
is %h<a>.raku, '1', 'a Hash subscript is unchanged';
