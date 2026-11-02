use Test;

plan 6;

# A role forwarding its type parameter into a composed role
# (`role Q[::U] does R[U]`) binds R's `T` to the argument Q was composed
# with (#15588).
my role R[::T] {
    has $.x;
    submethod BUILD { $!x = T.new }
    method m { T.new }
    method tn { T.^name }
}
my role Q[::U] does R[U] { method un { U.^name } }
my class B does Q[Array] { }

is B.new.x.raku, '[]', 'a forwarded parameter reaches the composed role\'s BUILD';
is B.new.m.raku, '[]', 'and its methods';
is B.new.tn, 'Array', 'T is the forwarded type';
is B.new.un, 'Array', 'U is still bound in the forwarding role';

my class H does Q[Hash] { }
is H.new.m.raku, '{}', 'another argument binds independently';

my role Q2 does R[Array] { }
my class B2 does Q2 { }
is B2.new.x.raku, '[]', 'a literal argument keeps working';
