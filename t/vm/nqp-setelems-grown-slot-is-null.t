use Test;
use nqp;

# A slot `nqp::setelems` (or an `nqp::bindpos` past the end) grows in an
# IterationBuffer is a null, as on rakudo (#17662). `nqp::atpos` answers null
# for it; a List over the buffer reads it as `Nil` through `[i]` and as `Mu`
# through iteration, `.raku` and list assignment.

plan 12;

my $b := nqp::create(IterationBuffer);
nqp::setelems($b, 2);
is nqp::isnull(nqp::atpos($b, 0)), 1, 'a grown slot is null';
is $b.List.raku, '(Mu, Mu)', 'and iterates as Mu';

my $c := nqp::create(IterationBuffer);
nqp::setelems($c, 2);
nqp::bindpos($c, 3, 1);
my $l := $c.List;
is $l[0].^name, 'Nil', '[i] of a setelems slot reads Nil';
is $l[2].^name, 'Nil', '[i] of a bindpos gap reads Nil';
is $l.map(*.^name).join(' '), 'Mu Mu Mu Int', 'iteration reads Mu for both';
is $l.raku, '(Mu, Mu, Mu, 1)', '.raku';
my @a = $c.List;
is @a.raku, '[Mu, Mu, Mu, 1]', 'list assignment keeps Mu';

nqp::push($c, Mu);
is nqp::isnull(nqp::atpos($c, 4)), 0, 'a pushed Mu is a value, not a null';
is nqp::isnull(nqp::atpos($c, 1)), 1, 'the holes stay null after a push';
is nqp::isnull(nqp::shift($c)), 1, 'a shifted hole is null';
is nqp::isnull(nqp::atpos($c, 0)), 1, 'the record follows the shift';

my $d := nqp::create(IterationBuffer);
nqp::setelems($d, 3);
nqp::setelems($d, 1);
nqp::push($d, 5);
is $d.List.raku, '(Mu, 5)', 'shrinking keeps the remaining hole';
