use Test;

plan 2;

# An allomorph (IntStr) indexes by its numeric part in ASSIGN-POS.
my @a;
my $i = <1>;
is-deeply @a.ASSIGN-POS($i, 'x'), 'x', 'ASSIGN-POS accepts an IntStr index';
is @a[1], 'x', 'it stored at the numeric position';
