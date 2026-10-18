use Test;

plan 5;

my $s = (1,2).Seq;
is-deeply ($s eqv Any), False, 'a Seq eqv a type object is False';
is $s.elems, 2, 'and the Seq is not consumed by it';

my @t = (1,2).Seq, (3,4).Seq;
my @u = @t.map(*.Seq);
is-deeply (@u[0] eqv Any), False, 'same for a Seq produced by map';
is @u[0].elems, 2, 'and it stays usable';

my $a = (1,2).Seq;
my $b = (1,2).Seq;
$a eqv $b;
throws-like { $a.List }, X::Seq::Consumed, 'two Seqs compared with each other are still consumed';
