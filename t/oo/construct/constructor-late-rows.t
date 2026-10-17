use Test;

# The constructors and `Failure.handled` are late rows of the receiver's MRO
# (ADR-11276 §9.141). Answers checked against Rakudo.

plan 12;

is Range.new(1, 5).raku, '1..5', 'Range.new';
is Range.new(1, 5, :excludes-max).raku, '1..^5', 'Range.new with an adverb';
class Foo { has $.x }
is Foo.new(x => 3).x, 3, 'Mu.new';
is Foo.bless(x => 4).x, 4, 'Mu.bless';
class Bar { has $.y; method new($y) { self.Mu::new(:$y) } }
is Bar.new(7).y, 7, 'the qualified Mu::new';
is-deeply Set.new-from-pairs("a" => 1, "b" => 0).keys.List, ('a',), 'Set.new-from-pairs';
is Mix.new-from-pairs("a" => 1.5).raku, '("a"=>1.5).Mix', 'Mix.new-from-pairs';
is Bag.new-from-pairs("a" => 2).raku, '("a"=>2).Bag', 'Bag.new-from-pairs';
{
    my $f = Failure.new("x");
    nok $f.handled, 'a fresh Failure is not handled';
    $f.handled = True;
    ok $f.handled, 'Failure.handled is settable';
}
dies-ok { Int.new-from-pairs(1) }, 'new-from-pairs is the QuantHashes\'';
is IO::Special.new('<STDOUT>').^name, 'IO::Special', 'IO::Special.new stays its own';
