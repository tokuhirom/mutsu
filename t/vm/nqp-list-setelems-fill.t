use v6;
use nqp;
use Test;

# `nqp::list_i` / `list_n` / `list_s` are native arrays, `nqp::list` an object
# one (#9235). Growing a native array -- `nqp::setelems`, or a `bindpos*` past
# the end -- opens slots that read back as that type's zero; growing an object
# array opens null slots. mutsu used to back all four with the same list and
# fill every gap with 0. Expected values were measured with rakudo.

plan 12;

my $o := nqp::list(1);
nqp::setelems($o, 3);
is nqp::isnull(nqp::atpos($o, 2)), 1, 'setelems grows nqp::list with null';
is nqp::elems($o), 3, '... to the requested size';

my $i := nqp::list_i(1);
nqp::setelems($i, 3);
is nqp::atpos_i($i, 2), 0, 'setelems grows nqp::list_i with 0';

my $n := nqp::list_n(1e0);
nqp::setelems($n, 3);
is nqp::atpos_n($n, 2), 0e0, 'setelems grows nqp::list_n with 0e0';

my $s := nqp::list_s('a');
nqp::setelems($s, 3);
is nqp::isnull_s(nqp::atpos_s($s, 2)), 1, 'setelems grows nqp::list_s with the null string';

my $o2 := nqp::list(1);
nqp::bindpos($o2, 3, 7);
is nqp::isnull(nqp::atpos($o2, 1)), 1, 'bindpos past the end of nqp::list leaves null gaps';

my $i2 := nqp::list_i(1);
nqp::bindpos_i($i2, 3, 7);
is nqp::atpos_i($i2, 1), 0, 'bindpos_i past the end of nqp::list_i leaves 0 gaps';

# The element kind belongs to the array: it survives a clone and pushes.
my $c := nqp::clone($i);
nqp::setelems($c, 6);
is nqp::atpos_i($c, 5), 0, 'a clone of nqp::list_i grows with 0';

my $e := nqp::list_i();
nqp::push_i($e, 1);
nqp::setelems($e, 3);
is nqp::atpos_i($e, 2), 0, 'an empty nqp::list_i pushed onto grows with 0';

my $l := nqp::list_i(1, 2, 3, 4);
nqp::setelems($l, 2);
is nqp::elems($l), 2, 'setelems shrinks nqp::list_i';
nqp::setelems($l, 4);
is nqp::atpos_i($l, 3), 0, '... and regrowing it fills with 0, not the dropped elements';

nqp::setelems($o, 6);
is nqp::isnull(nqp::atpos($o, 5)), 1, 'regrowing nqp::list fills with null';
