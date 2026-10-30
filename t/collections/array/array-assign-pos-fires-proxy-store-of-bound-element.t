use Test;

plan 4;

# `Array.ASSIGN-POS` on a slot BIND-POS-bound to a Proxy mediates the store
# through the Proxy's STORE instead of replacing the slot.
my @stored;
my $p := Proxy.new(
    FETCH => method { 10 },
    STORE => method (Mu $v) { @stored.push: $v },
);
my @a;
@a.BIND-POS(0, $p);
is @a.ASSIGN-POS(0, 5), 10, 'ASSIGN-POS answers the fetched value';
is-deeply @stored, [5], "the Proxy's STORE ran with the assigned value";
is @a[0], 10, 'the slot still reads through the Proxy';

my @b;
@b.ASSIGN-POS(1, 7);
is @b[1], 7, 'a plain slot is still just assigned';
