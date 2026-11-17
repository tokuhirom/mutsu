use nqp;
use Test;

# #17658: `nqp::push`, `nqp::unshift` and `nqp::bindkey` store their value
# operand as given, as `nqp::bindpos` does (#12786): a `$` variable is its
# Scalar container, so a later assignment to it shows in the slot. Expected
# values from rakudo (probed inside a routine, where no optimizer deconts).

plan 8;

sub probe() {
    my $l := nqp::list();
    my $x = 5;
    nqp::push($l, $x);
    $x = 6;
    is nqp::atpos($l, 0), 6, 'nqp::push binds the Scalar';
    ok nqp::iscont(nqp::atpos($l, 0)), 'the pushed slot is a container';

    my $y = 1;
    nqp::unshift($l, $y);
    $y = 2;
    is nqp::atpos($l, 0), 2, 'nqp::unshift binds the Scalar';

    my $h := nqp::hash();
    my $z = 'a';
    nqp::bindkey($h, 'k', $z);
    $z = 'b';
    is nqp::atkey($h, 'k'), 'b', 'nqp::bindkey binds the Scalar';

    my $b := IterationBuffer.new;
    my $w = 5;
    nqp::push($b, $w);
    $w = 7;
    is $b.List, (7,), 'an IterationBuffer slot follows the Scalar too';

    nqp::push($l, nqp::decont($x));
    $x = 9;
    is nqp::atpos($l, nqp::elems($l) - 1), 6, 'a deconted operand is a value';

    nqp::push($l, 42);
    is nqp::atpos($l, nqp::elems($l) - 1), 42, 'a literal is a value';
}
probe();

my $n = 0;
my $l := nqp::list();
sub hot() { my $v = 1; nqp::push($l, $v); $v = 2; nqp::atpos($l, nqp::elems($l) - 1) }
$n += hot() for ^200;
is $n, 400, 'a hot routine binds the Scalar as well';
