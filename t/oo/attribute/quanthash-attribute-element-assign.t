use v6;
use Test;

# A SetHash/BagHash held in an attribute is mutated by an element store written
# through the accessor or through `Attribute.get_value`. Found by
# RedX::HashedPassword (Red's `$!dirty-cols-attr.get_value(obj).{$_}++`).

plan 9;

class Holder {
    has $.seen  = SetHash.new;
    has $.count = BagHash.new;
    has $.frozen = Set.new(<a>);
}

my $h = Holder.new;

$h.seen<k> = True;
is $h.seen.elems, 1, 'accessor subscript assignment adds to a SetHash';
$h.seen<j>++;
is $h.seen.elems, 2, 'accessor subscript ++ adds to a SetHash';
$h.seen<k> = False;
is $h.seen.elems, 1, 'a false value removes the element';

$h.count<x>++;
$h.count<x>++;
is $h.count<x>, 2, 'accessor subscript ++ counts in a BagHash';

throws-like { $h.frozen<z> = True }, X::Assignment::RO, 'an immutable Set refuses the store';

my $seen = Holder.^attributes.first(*.name eq '$!seen');
$seen.get_value($h){"m"} = True;
is $h.seen.elems, 2, 'get_value(...){key} = True adds to the SetHash';
$seen.get_value($h).{"n"}++;
is $h.seen.elems, 3, 'get_value(...).{key}++ adds to the SetHash';

my $count = Holder.^attributes.first(*.name eq '$!count');
$count.get_value($h){"y"}++;
is $h.count<y>, 1, 'get_value(...){key}++ counts in the BagHash';
$count.get_value($h){"y"} = 5;
is $h.count<y>, 5, 'get_value(...){key} = n sets the BagHash weight';
