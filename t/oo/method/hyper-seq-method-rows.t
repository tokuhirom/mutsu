use Test;

# `HyperSeq`'s and `RaceSeq`'s own methods are rows of the method table,
# found by their owner from the interpreter and from both VM call paths
# (ADR-11276 §9.137). Answers checked against Rakudo.

plan 12;

my $h = (1..6).hyper(:batch(2));
is $h.^name, 'HyperSeq', 'hyper with an adverb';
is $h.configuration.batch, 2, 'configuration through a variable';
is (1..6).hyper(:batch(3)).configuration.batch, 3, 'configuration on a temporary';
is $h.map(* * 2).^name, 'HyperSeq', 'map stays a HyperSeq';
is $h.grep(* > 3).^name, 'HyperSeq', 'grep stays a HyperSeq';
is $h.is-lazy, False, 'is-lazy';
is (1..4).hyper.race.^name, 'RaceSeq', 'race on a HyperSeq';
is-deeply (1..3).race.list, (1, 2, 3), 'list reads the sequence';
{
    my $i = (1..3).hyper;
    $i.iterator;
    throws-like { $i.iterator }, X::Seq::Consumed, 'a second iterator';
}
throws-like { (1..3).hyper(:batch(0)) }, X::Invalid::Value,
    message => "Invalid value '0' for :batch on method hyper", 'a non-positive batch';
is-deeply (1..4).hyper.map(* + 1).sort.List, (2, 3, 4, 5), 'map then a List method';
is (1..4).hyper.elems, 4, 'elems reads the List';
