use Test;

plan 3;

my @values = 1..100;
my $seq = Seq.from-loop({ @values.shift }, { state $count = 0; $count++ < 10 });
is-deeply $seq, (1..10).Seq, 'from-loop body and condition retain closure-private capture state';

my $count = 0;
$seq = Seq.from-loop({ ++$ }, { $count < 10 }, { $count++ });
is-deeply $seq, (1..10).Seq, 'from-loop step closure updates captured lexical across iterations';
is $count, 10, 'from-loop step writes persist to the captured lexical';
