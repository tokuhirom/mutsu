use Test;

# An element store through an `@` attribute's accessor past the end
# (`$y.d[2] = 1`) leaves the skipped slots as holes, as a store into a
# variable does: `:exists` is False and `.List` reads `Nil` there until a
# later store fills them. Expected values from rakudo (#17778).

plan 9;

class Y { has Int @.d; has @.u }

my $y = Y.new(d => (3,));
$y.d[2] = 1;
nok $y.d[1]:exists, 'the skipped slot is a hole';
ok $y.d[2]:exists, 'the stored slot exists';
ok $y.d[0]:exists, 'the slot that was there exists';
is-deeply $y.d.List, (3, Nil, 1), '.List reads the hole as Nil';
is $y.d.raku, 'Array[Int].new(3, Int, 1)', '.raku shows the element type object';

$y.d[1] = 5;
ok $y.d[1]:exists, 'a later store fills the hole';
is-deeply $y.d.List, (3, 5, 1), '...and is read back';

my $u = Y.new;
$u.u[3] = 'x';
is-deeply ($u.u[0]:exists, $u.u[3]:exists, $u.u.elems), (False, True, 4),
    'an untyped attribute array grown from empty';

throws-like { Y.new.d = (1, Any, 3) }, X::TypeCheck::Assignment,
    message => 'Type check failed for an element of @!d; expected Int but got Any (Any)',
    'assigning a list with a wrong element to a typed attribute array';
