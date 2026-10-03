use v6;
use Test;
use nqp;

# nqp::getattr on a Capture: `@!list` is its positionals and `%!hash` its
# nameds. Upstream NativeCall reads `@!list` off the `|c` of an `is native`
# routine's replacement body to build the C call's arguments (#11203).

plan 6;

my $f = -> |c {
    my Mu $list := nqp::getattr(nqp::decont(c), Capture, '@!list');
    my Mu $hash := nqp::getattr(nqp::decont(c), Capture, '%!hash');
    (nqp::elems($list), nqp::atpos($list, 0), nqp::atpos($list, 1), $hash<k>)
};

my ($elems, $first, $second, $named) = $f('x', 2, :k(3));
is $elems, 2, '@!list holds the positionals';
is $first, 'x', 'first positional';
is $second, 2, 'second positional';
is $named, 3, '%!hash holds the nameds';

my $g = -> |c { nqp::elems(nqp::getattr(nqp::decont(c), Capture, '@!list')) };
is $g(), 0, 'an empty capture has an empty @!list';

my $c = \(1, 2, 3);
is nqp::elems(nqp::getattr(nqp::decont($c), Capture, '@!list')), 3, 'a literal capture too';
