use Test;

# `.AT-POS($i)` answers what the subscript `[$i]` answers, for a plain Array
# and List: an element, a hole, past the end, a bound element cell.

plan 9;

my @a = 10, 20, 30;
is @a.AT-POS(1), 20, 'an element of an Array';
is @a.AT-POS(5), Any, 'past the end of an Array is Any';
is @a.AT-POS(5).raku, @a[5].raku, 'and agrees with the subscript';

my @holes;
@holes[3] = 1;
is @holes.AT-POS(1), Any, 'a hole of an Array is Any';

my $l = (1, 2, 3);
is $l.AT-POS(2), 3, 'an element of a List';
is-deeply $l.AT-POS(9), Nil, 'past the end of a List is Nil';

my $x = 5;
my @b;
@b[0] := $x;
is @b.AT-POS(0), 5, 'a bound element reads its container';
$x = 6;
is @b.AT-POS(0), 6, 'and follows the container';

sub scan(\haystack) {
    my $sum = 0;
    for ^haystack.elems -> $i { $sum += haystack.AT-POS($i) }
    $sum
}
is scan(@a), 60, 'through a sigilless parameter in a loop';
