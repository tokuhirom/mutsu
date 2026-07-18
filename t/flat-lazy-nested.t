use v6;
use Test;

# flat on a genuinely-lazy list must flatten nested arrays while staying
# lazy (pin for the lazy flattening pipe; roast/S02-types/lazy-lists.t
# test 27 is the upstream case).

plan 6;

{
    my @l = flat [2, 3, 4], 10, 11 ... *;
    is ~@l[^6], "2 3 4 10 11 12", 'flat spills a nested array inside an infinite sequence';
}

{
    my $x = flat [2, 3, 4], 10, 11 ... *;
    ok $x.is-lazy, 'flat of an infinite sequence stays lazy';
    isa-ok $x, Seq, 'flat of an infinite sequence is a Seq';
}

{
    is ~(flat [2, 3, 4], 10, 11 ... 15), "2 3 4 10 11 12 13 14 15",
        'finite sequence flat still flattens';
}

{
    is ~(flat 42 xx *).head(3), "42 42 42", 'flat of xx * stays lazy and yields elements';
}

{
    my $g = flat lazy gather { take [1, 2]; take 3 };
    is ~$g[^3], "1 2 3", 'flat of a lazy gather flattens its nested arrays';
}
