use Test;

# An outer type object reached past a same-named lexical type is matched and
# dispatched as itself, not re-resolved through the block's declaration
# (#18233).

plan 8;

class Inner { method tag { "outer" } }
role Ro { method r { "outer-role" } }
my $outer-ro = Ro;

sub b() {
    {
        my class Inner { method tag { "block" } }
        my role Ro { method r { "block-role" } }
        my $o = OUTER::OUTER::Inner;
        nok $o ~~ Inner, 'the outer class type object does not match the block class';
        nok Inner.ACCEPTS($o), '... through ACCEPTS either';
        my $p = -> Inner $x { 1 };
        is (try $p($o)) // 'died', 'died', '... nor binds to a parameter typed with it';
        is OUTER::OUTER::Ro.r, 'outer-role', 'a method on the outer role type object puns the outer role';
        is $outer-ro.r, 'outer-role', '... also through a variable holding it';
        is Ro.r, 'block-role', 'the block role puns itself';
    }
}
b();
is Ro.r, 'outer-role', 'the outer role after the block';
is Inner.tag, 'outer', 'the outer class after the block';
