use Test;

plan 8;

# `.Seq` never iterates a lazy source (#13254).
my $s := 1...*;
my $t = $s.Seq;
is $t.^name, 'Seq', '.Seq on an infinite lazy Seq is a Seq';
ok $t.is-lazy, '... and stays lazy';
is $t[^3].join(','), '1,2,3', '... and still yields its elements';

my $l = (1...*).List.Seq;
is $l.^name, 'Seq', '.Seq on a lazy .List is a Seq';
ok $l.is-lazy, '... and stays lazy';

my $a = (1...*).Array.Seq;
is $a.^name, 'Seq', '.Seq on a lazy .Array is a Seq';

is (1..3).map(* * 2).Seq.join(','), '2,4,6', '.Seq on a finite map Seq';
is (1..Inf).map(* * 2).Seq.is-lazy, True, '.Seq on an infinite map Seq stays lazy';
