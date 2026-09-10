use v6;
use Test;

# Buf/Blob are Positional: list context yields the elements, so .rotor
# batches bytes and `for` iterates them (Base64's encoder rotors a Blob).

plan 7;

# The one-arg FLATTEN rule is different from list coercion: Buf/Blob are
# Positional but not Iterable, so a composer keeps them whole (rakudo).
is [Blob.new(1, 2)].elems, 1, '[$blob] is one element';
my @assigned = Blob.new(1, 2);
is @assigned.elems, 1, 'my @a = $blob is one element';

is-deeply Blob.new(65).rotor(3, :partial).head, (65,),
    'Blob.rotor iterates bytes, not the Blob as one item';
is-deeply Buf.new(1, 2, 3, 4).rotor(2).elems, 2, 'Buf.rotor batches bytes';

my @seen;
for Buf.new(7, 8) -> $b { @seen.push($b) }
is-deeply @seen, [7, 8], 'for over a Buf iterates its bytes';

is-deeply Blob.new(1, 2).pairs.map({ .key => .value }).List,
    (0 => 1, 1 => 2), 'Blob.pairs enumerates byte positions';

is Buf.new(10, 20, 30).List.elems, 3, 'Buf.List yields the elements';
