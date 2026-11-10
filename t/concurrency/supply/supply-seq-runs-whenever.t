use Test;

plan 4;

my $t = supply { whenever Supply.from-list(3, 4) { emit $_ * 2 } };
is-deeply $t.Seq.list.List, (6, 8), '.Seq of a supply with a whenever taps the whenever source';

my @got;
my $s = supply { whenever Supply.from-list(6) { @got.push: $_ } }.Seq;
$s.sink;
is-deeply @got, [6], 'sinking the Seq runs the whenever block';

@got = ();
react whenever Supply.from-list(1) {
    supply { whenever Supply.from-list(5) { @got.push: $_ } }.Seq
}
is-deeply @got, [5], 'a supply Seq returned from a react whenever block is consumed';

is-deeply (supply { emit 1; emit 2 }).Seq.list.List, (1, 2), 'plain emit still works';
