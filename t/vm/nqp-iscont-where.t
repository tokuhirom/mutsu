use Test;
use nqp;

# #9346: `nqp::iscont` (is the operand a container) and `nqp::where` (the
# object's identity integer, i.e. `.WHERE`). Expected values measured with
# rakudo 2026.07.

plan 19;

my $x = 1;
my @a = 1, 2;
my %h = a => 1;

is nqp::iscont($x), 1, 'a scalar variable is a container';
is nqp::iscont(1), 0, 'a literal is not';
is nqp::iscont(@a[0]), 1, 'an array element is a container';
is nqp::iscont(%h<a>), 1, 'a hash element is a container';
is nqp::iscont(@a), 0, 'an array variable is not a Scalar container';
is nqp::iscont($x<>), 0, 'a decontainerized value is not';

# Parameters. `f` is small enough for the typed fast path (TRIR), which must
# not answer from the bare value.
sub f($p) { nqp::iscont($p) }
sub g($p is rw) { nqp::iscont($p) }
sub h(\p) { nqp::iscont(p) }
is f($x), 1, 'a readonly parameter bound to a variable keeps its container';
is g($x), 1, 'an is-rw parameter';
is h($x), 1, 'a raw parameter bound to a variable';
is h(1), 0, 'a raw parameter bound to a literal, after one bound to a variable';
my \s = 5;
is nqp::iscont(s), 0, 'a sigilless binding to a value';

# Scalar::Util's `readonly` is exactly this.
sub readonly(\a) { nqp::hllbool(nqp::not_i(nqp::iscont(a))) }
ok readonly(1), 'readonly(1)';
nok readonly($x), 'readonly($x)';

my $o = [1];
my $p = [1];
ok nqp::where($o) > 0, 'where is a positive integer';
is nqp::where($o), nqp::where($o), 'where is stable for one object';
is nqp::where($o), $o.WHERE, 'where agrees with .WHERE';
isnt nqp::where($o), nqp::where($p), 'where differs between live objects';
sub w($v) { nqp::where($v) }
is w($o), $o.WHERE, 'where through a parameter';
isa-ok nqp::where($o), Int, 'where is an Int';
