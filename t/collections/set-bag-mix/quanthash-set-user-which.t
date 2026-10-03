use Test;

# `SetHash.set`/`.unset` key an object by its user-defined `WHICH`, and `.set`
# of an element already present stores the new object (rakudo rebinds the
# key). They used to key by the per-object identity, so two objects with the
# same `WHICH` became two elements (CRDT's LWW-Element-Set).

plan 5;

class Item {
    has $.value;
    has $.ts;
    method WHICH { $!value.WHICH }
}

my %s is SetHash;
%s.set: Item.new(:value<d>, :ts(1));
%s.set: Item.new(:value<d>, :ts(2));
is %s.elems, 1, 'one element per WHICH';
is %s.keys.map(*.ts).List, (2,), '.set stores the newer object';

%s.unset: Item.new(:value<d>, :ts(9));
is %s.elems, 0, '.unset finds the element by WHICH';

my $h = SetHash.new;
$h.set: Item.new(:value<e>, :ts(1));
$h{Item.new(:value<e>, :ts(5))} = True;
is $h.elems, 1, 'subscript assignment agrees with .set';
is $h.keys.map(*.ts).List, (5,), 'and also stores the newer object';
