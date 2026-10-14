# `nqp::p6bindattrinvres(nqp::create(Capture), Capture, '@!list', $list)`
# builds a Capture over `$list` (and `'%!hash'` its nameds). Found in
# ValueList, whose `.Capture` is exactly that bind on `self`.
use Test;
use nqp;

plan 6;

my $empty := nqp::create(Capture);
is $empty.raku, '\()', 'nqp::create(Capture) is an empty Capture';

my $c := nqp::p6bindattrinvres(nqp::create(Capture), Capture, '@!list', nqp::list(1, 2, 3));
isa-ok $c, Capture, 'p6bindattrinvres answers a Capture';
is-deeply $c.list, (1, 2, 3), 'its positionals are the bound list';

my $d := nqp::p6bindattrinvres($c, Capture, '%!hash', nqp::hash('a', 1));
is-deeply $d.list, (1, 2, 3), 'binding %!hash keeps the positionals';
is $d<a>, 1, 'and adds the nameds';

class Buffered is IterationBuffer does Iterable is repr('VMArray') {
    method Capture() { nqp::p6bindattrinvres(nqp::create(Capture), Capture, '@!list', self) }
}
my $b := nqp::create(Buffered);
nqp::push($b, $_) for 4, 5;
is-deeply $b.Capture.list, (4, 5), 'a VMArray-repr object as the positionals';
