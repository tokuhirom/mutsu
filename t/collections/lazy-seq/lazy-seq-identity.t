use Test;

plan 9;

# A lazy Seq is a reference type: `===` and `.WHICH` answer by the one object and
# never force it (forcing `1...*` used to die with "Cannot coerce an infinite
# lazy list to a strict list").
my $s := 1...*;
ok $s === $s, "an infinite lazy Seq is === itself";
ok $s.WHICH eq $s.WHICH, ".WHICH is stable across calls";
nok $s !=== $s, "!=== of a lazy Seq with itself is False";
nok $s === (1...*), "two separately built lazy Seqs are not ===";

my $g = gather { take 1 };
ok $g === $g, "a gather Seq is === itself";
ok $g.WHICH eq $g.WHICH, "...and its .WHICH is stable";

my $m = (1..3).map({ $_ });
ok $m === $m, "a map Seq is === itself";
nok $m === (1..3).map({ $_ }), "two separately built map Seqs are not ===";

my $other := 2...*;
nok $s === $other, "different lazy Seqs are not ===";
