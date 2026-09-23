use Test;

plan 1;

sub bump-pairs(*@args is raw) {
    my int $i;
    while $i < @args.elems {
        my \a := @args[$i++];
        my \b := $i < @args.elems ?? @args[$i++] !! Nil;
        b++ if $i <= @args.elems;
    }
}

my @pairs = 'one', 1, 'two', 2;
bump-pairs @pairs;
is-deeply @pairs, ['one', 2, 'two', 3],
    'a sigilless alias can bind a raw slurpy element selected by a conditional';
