use Test;
use NativeCall;

# A `:=`-bound CArray slice is the Seq the imported `postcircumfix:<[ ]>`
# returns, not a List of cells (#16546). Expectations measured against rakudo.

plan 7;

my $c = CArray[num64].new;
$c[$_] = $_ * 1.5e0 for ^6;

my $q := $c[1, 3];
is $q.WHAT.^name, 'Seq', 'a bound slice is a Seq';
is $q.raku, '(1.5e0, 4.5e0).Seq', 'and renders as one';

my $r := $c[(1, 3)];
is $r.WHAT.^name, 'Seq', 'a parenthesized index list too';

my $s := $c[1,];
is $s.raku, '(1.5e0,).Seq', 'a one-element slice';

my $range := $c[1..2];
is $range.WHAT.^name, 'Seq', 'a range slice';

my @a = 1..6;
my $bound := @a[1, 3];
is $bound.raku, '(2, 4)', 'a plain Array slice still binds a List';
$bound[0] = 9;
is @a[1], 9, 'and writes through the bound cell';
