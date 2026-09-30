use Test;

# From the Directory distribution: `@entries.push: $iodir.dir.Slip` where
# `.dir` returns a not-yet-forced `gather` Seq.

plan 5;

my $s = gather { take 1; take 2 };
is-deeply $s.Slip.raku, 'slip(1, 2)', '.Slip on an unforced gather Seq yields its elements';

my @a;
@a.push: (gather { take 1; take 2 }).Slip;
is-deeply @a, [1, 2], 'push of a gather Seq .Slip flattens';

my @b = (gather { take 3; take 4 }).Slip;
is-deeply @b, [3, 4], 'list assignment from gather .Slip';

my $t = gather { take 5; take 6 };
$t.Slip;
is-deeply $t.Slip.elems, 2, '.Slip twice is repeatable';

is-deeply (gather { }).Slip.elems, 0, 'empty gather slips to nothing';

done-testing;
