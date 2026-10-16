use Test;

# A `for` over a `$`-held mutable QuantHash's `.values`/`.kv`/`.pairs` writes
# the updated container back to the variable. Inside a closure another routine
# runs, that write reaches the variable through the closure's capture cell,
# not through a frame's return merge (ADR-12529 §3, slice 30). Checked against
# rakudo.

plan 5;

sub run(&c) { c() }

{
    my $b = <a>.BagHash;
    run { $_ = 7 for $b.values };
    is $b<a>, 7, 'a BagHash weight set through .values';
}

{
    my $s = <x y>.SetHash;
    run { for $s.kv -> \k, \v { v = False if k eq 'x' } };
    is $s.keys.sort.join, 'y', 'a SetHash key dropped through .kv';
}

{
    my $m = (a => 1.5).MixHash;
    run { .value = 2.5 for $m.pairs };
    is $m<a>, 2.5, 'a MixHash weight set through .pairs';
}

{
    my $b = <a b>.BagHash;
    my &peek = { $b.total };
    run { $_ = 3 for $b.values };
    is peek(), 6, 'a closure made before the loop sees the new weights';
}

{
    my $b = <a>.BagHash;
    run { $_ = 0 for $b.values };
    is $b.elems, 0, 'a zero weight removes the key';
}
