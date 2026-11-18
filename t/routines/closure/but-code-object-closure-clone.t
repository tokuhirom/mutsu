use Test;

# `$code but R` mixes the role into a closure clone of `$code` (rakudo's `but`
# is `.clone.^mixin`), so the result has its own `state` variables and its own
# `FIRST` flag, as `$code.clone` does. mutsu mixed into the original code
# object itself, so the mixin shared both with it. Expected values checked
# against raku.

plan 5;

my role R { }

{
    my $blk = { state $n = 0; $n++ };
    is ($blk(), $blk()).join, '01', 'the original counts from 0';
    my $m = $blk but R;
    is ($m(), $m()).join, '01', 'the mixin has its own state variables';
    is $blk(), 2, 'the original keeps its own state';
}

{
    my $fired = 0;
    my $blk = { FIRST $fired++; $_ };
    (^2).map($blk).List.elems;
    is $fired, 1, 'FIRST fires once for the original';
    (^2).map($blk but R).List.elems;
    is $fired, 2, 'the mixin is a fresh clone, so its FIRST fires again';
}
