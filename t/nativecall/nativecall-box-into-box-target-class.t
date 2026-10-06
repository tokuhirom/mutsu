use Test;
use nqp;

# #11209 (ADR-11203 §2.4): `nqp::box_i/n/s/u` into a class, and `nqp::unbox_*`
# out of one. MoarVM allocates an object of the type operand and puts the
# native value wherever that type keeps one: a class with an `is box_target`
# attribute stores it there. Every expected value here is rakudo's.

plan 27;

my class IntBox { has int $!v is box_target; method v() { $!v } }
my class NumBox { has num $!v is box_target; method v() { $!v } }
my class StrBox { has str $!v is box_target; method v() { $!v } }
my class UIntBox { has uint $!v is box_target; method v() { $!v } }

{
    my $i := nqp::box_i(42, IntBox);
    isa-ok $i, IntBox, 'nqp::box_i into the class gives an instance of it';
    is $i.v, 42, 'holding the value in its box target';
    is nqp::unbox_i($i), 42, 'nqp::unbox_i reads it back';
    is nqp::unbox_i(IntBox.new), 0, 'an object that boxed nothing unboxes to the seed';
}

{
    my $n := nqp::box_n(2.5e0, NumBox);
    isa-ok $n, NumBox, 'nqp::box_n into the class gives an instance of it';
    is $n.v, 2.5, 'holding the value in its box target';
    is nqp::unbox_n($n), 2.5, 'nqp::unbox_n reads it back';
}

{
    my $s := nqp::box_s("hi", StrBox);
    isa-ok $s, StrBox, 'nqp::box_s into the class gives an instance of it';
    is $s.v, 'hi', 'holding the value in its box target';
    is nqp::unbox_s($s), 'hi', 'nqp::unbox_s reads it back';
}

{
    my $u := nqp::box_u(7, UIntBox);
    isa-ok $u, UIntBox, 'nqp::box_u into the class gives an instance of it';
    is $u.v, 7, 'holding the value in its box target';
    is nqp::unbox_u($u), 7, 'nqp::unbox_u reads it back';
    is nqp::unbox_u(nqp::box_u(-1, UIntBox)), 18446744073709551615, 'an unsigned box wraps -1 to 2**64 - 1';
}

# Two boxes are two objects.
{
    my $a := nqp::box_i(1, IntBox);
    my $b := nqp::box_i(2, IntBox);
    is $a.v, 1, 'the first box keeps its own value';
    is $b.v, 2, 'beside the second';
    ok $a.WHERE != $b.WHERE, 'they are separate objects';
}

# The box target is found through inheritance and through a composed role.
{
    my class IntBoxChild is IntBox { }
    my $c := nqp::box_i(9, IntBoxChild);
    isa-ok $c, IntBoxChild, 'a subclass boxes as itself';
    is nqp::unbox_i($c), 9, 'and unboxes through the inherited box target';

    my role Holder { has int $!v is box_target; method v() { $!v } }
    my class Held does Holder { }
    my $h := nqp::box_i(5, Held);
    isa-ok $h, Held, 'a class that composes a role with a box target boxes as itself';
    is $h.v, 5, 'holding the value in the role attribute';
    is nqp::unbox_i($h), 5, 'and unboxes through it';
}

# Types with no boxing of their own keep answering the plain value.
{
    my class Plain { has int $.v }
    is nqp::box_i(3, Int), 3, 'nqp::box_i into Int is the Int';
    is nqp::box_s('x', Str), 'x', 'nqp::box_s into Str is the Str';
    is nqp::box_n(1.5e0, Num), 1.5, 'nqp::box_n into Num is the Num';
    is nqp::unbox_i(5), 5, 'nqp::unbox_i of a plain Int is the Int';
    is nqp::unbox_s('abc'), 'abc', 'nqp::unbox_s of a plain Str is the Str';
}
