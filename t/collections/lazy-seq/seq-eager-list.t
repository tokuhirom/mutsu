use Test;
plan 5;

my $s = (1,2).map(* + 1);
my $e = $s.eager;
my $s2 = (1,2).map(* + 1);
is $e.^name, 'List', 'Seq.eager answers a List';
is $s2.eager.raku, '(2, 3)', 'Seq.eager .raku has no .Seq suffix';
ok (1,2).map(* + 1).eager ~~ List, 'Seq.eager ~~ List';
is (1,2).eager.^name, 'List', 'List.eager stays a List';
is [1,2].eager.^name, 'Array', 'Array.eager stays an Array';
