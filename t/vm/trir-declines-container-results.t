use Test;
use nqp;

plan 6;

# A routine whose body hands out a container leaves the typed tier (TRIR),
# whose values carry no container, to the bytecode path (#18080): `return-rw`
# returns the container it names, and `nqp::push` / `bindkey` bind a `$`
# variable's Scalar. Typed and parameterless routines both reach TRIR once
# their calls are cached, so each is called repeatedly.

my $v = 1;
sub rw-typed(Int $x) { return-rw $v }
sub rw-bare() { return-rw $v }
is (rw-typed(1) for ^3).join(','), '1,1,1', 'return-rw from a typed routine reads the value';
rw-typed(1) = 5 for ^3;
is $v, 5, 'assigning to its result writes the variable';
rw-bare() = 7 for ^3;
is $v, 7, 'the same from a parameterless routine';

my $l := nqp::list();
sub hot-typed(Int $x) { my $w = 1; nqp::push($l, $w); $w = $x; nqp::atpos($l, nqp::elems($l) - 1) }
my $n = 0;
$n += hot-typed(2) for ^50;
is $n, 100, 'nqp::push in a typed routine binds the Scalar';

my $h := nqp::hash();
sub key-typed(Str $k) { my $w = 'a'; nqp::bindkey($h, $k, $w); $w = 'b'; nqp::atkey($h, $k) }
is (key-typed('k') for ^3).join(','), 'b,b,b', 'nqp::bindkey in a typed routine binds the Scalar';

sub pushed-literal(Int $x) { nqp::push($l, $x * 2); nqp::atpos($l, nqp::elems($l) - 1) }
is (pushed-literal(3) for ^3).join(','), '6,6,6', 'a pushed value that is no container';
