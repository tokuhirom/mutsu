use v6;
use Test;
use nqp;

# A lexical `:=`-bound to an immutable value can be re-bound from a sub that
# sees it as an outer. The rebind reaches SetGlobal, whose readonly check used
# to reject it with "Cannot assign to an immutable value" (#9238). Expected
# values were measured with rakudo.

plan 6;

package Q {
    my $tbl := nqp::list_i(1, 2);
    our sub probe(int $i) { nqp::atpos_i($tbl, $i) }
    our sub rebind() { $tbl := nqp::list_i(5, 6); Nil }
}
is Q::probe(0), 1, 'a package lexical bound to a native list';
lives-ok { Q::rebind() }, 'can be re-bound from a sub in the same package';
is Q::probe(0), 5, '... and the rebind is seen';

my $y := 5;
sub ry() { $y := 2; Nil }
lives-ok { ry() }, 'a mainline lexical bound to an Int can be re-bound from a sub';
is $y, 2, '... to the new value';
sub ay() { $y = 3 }
throws-like { ay() }, Exception, message => 'Cannot assign to an immutable value',
    'assigning to it still dies: it is bound to an immutable value again';
