use Test;

# The code object `.^find_method` / `.^lookup` hand back runs the method's
# compiled body, with its invocant, `callwith` and the subs nested in the body
# (#14572). Checked against rakudo.
plan 6;

role W {
    method _rw_wrapper($self: |c) is rw { callwith($self, |c); }
    method nested($self:) { sub inner($y) { $y + 1 }; inner(41) }
}
class B {
    has Str $.foo is rw;
    method add($a, $b) { $a + $b }
}
B.HOW does W;
B.^find_method('foo').wrap(B.HOW.^find_method('_rw_wrapper'));

my $b = B.new;
lives-ok { $b.foo = 'x' }, 'assignment through an accessor wrapped with a found role method';
is $b.foo, 'x', 'and the value is stored';

my $add = B.^find_method('add');
is $add(B.new, 2, 3), 5, 'a found method called as a code object';
is B.^lookup('add').(B.new, 4, 5), 9, 'so is a looked-up one';
is B.HOW.^find_method('nested')(B.HOW), 42, 'a sub nested in the found method resolves';
is B.HOW.^find_method('nested').name, 'nested', 'the code object keeps its name';
