use v6;
use Test;

plan 9;

# `.classify` / `.categorize` store each bucket as an itemized array (`$[...]`),
# so a bucket does not flatten when assigned to an `@`-array and iterates as a
# single element — matching Raku.

{
    my %c = (1, 2, 3, 4).classify(* %% 2);

    is %c{True}.elems, 2, 'bucket .elems sees the inner elements';
    is %c{True}[0],     2, 'bucket positional index works on the inner array';

    my @flat = %c{True};
    is @flat.elems, 1, 'assigning a bucket to @-array does not flatten (itemized)';

    my $iterations = 0;
    $iterations++ for %c{True};
    is $iterations, 1, 'iterating a bucket runs once (itemized array)';
}

{
    my %c = (1, 2, 3, 4).categorize({ $_ %% 2 ?? 'even' !! 'odd' });
    my @flat = %c<even>;
    is @flat.elems, 1, 'categorize bucket is itemized too';
    is %c<even>.elems, 2, 'categorize bucket inner elems';
    is-deeply %c<even>.List, (2, 4), 'categorize bucket contents';
}

# A bucket with a single element is still itemized (one, not zero flattening).
{
    my %c = (5,).classify({ 'only' });
    my @flat = %c<only>;
    is @flat.elems, 1, 'single-element bucket stays itemized';
    is %c<only>[0], 5, 'single-element bucket content';
}
