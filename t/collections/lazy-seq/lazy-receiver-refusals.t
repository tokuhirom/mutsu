use Test;

# On a lazy receiver (infinite, or marked `lazy`) Rakudo's counting, ordering
# and sampling methods refuse without iterating: a `Failure` of
# `X::Cannot::Lazy` naming the method and the type it would reify onto, or a
# death for `tail` and a counted `roll`. `values` is `self.list` and stays
# lazy. The answer is the same whether the receiver is an expression or a
# variable (both VM method lanes ask the lazy step's pulls). Every message
# was checked against Rakudo.

plan 25;

sub refusal(&code) {
    my $r;
    {
        $r = code();
        CATCH { default { return 'died: ' ~ .message } }
    }
    $r ~~ Failure ?? 'Failure: ' ~ $r.exception.message !! $r.raku
}

# A fresh infinite sequence each time: a Seq is consumed once.
sub inf() { (1..*).map(*+0) }
is refusal({ (1..*).map(*+0).sum }), 'Failure: Cannot .sum a lazy list onto a Seq', 'sum';
is refusal({ my $v = inf; $v.sum }), 'Failure: Cannot .sum a lazy list onto a Seq', 'sum through a variable';
is refusal({ (1, 2 ... *).reverse }), 'Failure: Cannot .reverse a lazy list onto a Seq', 'reverse';
is refusal({ (1..*).map(*+0).list.reverse }), 'Failure: Cannot reverse a lazy list onto a List',
    'List.reverse names no dot';
is refusal({ inf.pick }), 'Failure: Cannot .pick from a lazy list onto a List', 'pick';
is refusal({ inf.pick(2) }), 'Failure: Cannot .pick from a lazy list onto a List', 'pick(2)';
is refusal({ inf.roll }), 'Failure: Cannot .roll from a lazy list onto a List', 'roll';
is refusal({ inf.roll(2) }), 'died: Cannot .roll from a lazy list onto a List', 'roll(2) dies';
is refusal({ inf.elems }), 'Failure: Cannot .elems a lazy list', 'elems';
is refusal({ inf.Numeric }), 'Failure: Cannot .elems a lazy list', 'Numeric counts';
is refusal({ inf.Int }), 'Failure: Cannot .elems a lazy list', 'Int counts';
is refusal({ inf.end }), 'Failure: Cannot .end a lazy list', 'Seq.end';
is refusal({ inf.list.end }), 'died: Cannot .elems a lazy list', 'List.end dies';
is refusal({ inf.tail }), 'died: Cannot tail a lazy list', 'tail dies';
is refusal({ inf.tail(2) }), 'died: Cannot tail a lazy list', 'tail(2) dies';

my @a = 1, 2 ... *;
is refusal({ @a.sum }), 'Failure: Cannot .sum a lazy list onto a Array', 'an Array';
is refusal({ @a.pick }), 'Failure: Cannot .pick from a lazy list onto a Array', 'pick of an Array';

my $marked = lazy 1, 2, 3;
is refusal({ $marked.sum }), 'Failure: Cannot .sum a lazy list onto a Seq', 'a lazy-marked list';
is refusal({ (lazy 1, 2, 3).reverse }), 'Failure: Cannot .reverse a lazy list onto a Seq',
    '... reverse';
is (lazy 1, 2, 3).eager.sum, 6, 'eager first answers';

my $g = gather { take 1; take 2; take 3 };
is $g.sum, 6, 'a plain gather is not lazy, and sums';
is (1, 2, 3).map(*+0).reverse.List, (3, 2, 1), 'nor is a finite map';

is (1, 2 ... *).values.head(2), (1, 2), 'values of an infinite sequence';
ok (1, 2 ... *).values.is-lazy, '... stays lazy';
is (1, 2 ... *).values.^name, 'List', '... as a List';
