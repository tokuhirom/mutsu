use v6;
use Test;

# Bool is-an Int, so a Bool value (or the Bool type object) binds an `Int`
# parameter. The first call of a class-nested sub took the general binder and
# passed; the repeat call took the positional-light fast path, whose type
# check did not admit Bool for Int. Found with GD (`check-compat(Int $fill, ...)`
# called with a `Bool :$fill`). Identical under raku.

plan 8;

class Img {
    sub cc(Int $f) { $f }
    method m(Bool $b) { cc($b) }
    method n(Bool :$fill) { cc($fill) }
}

my $i = Img.new;
is $i.m(True), True, 'first call with True';
is $i.m(True), True, 'repeat call with True';
is $i.m(False), False, 'repeat call with False';
is $i.n(:fill), True, 'named Bool, repeated: first';
is $i.n(:fill), True, 'named Bool, repeated: second';
is $i.n.gist, '(Bool)', 'the Bool type object binds Int too (first)';
is $i.n.gist, '(Bool)', 'the Bool type object binds Int too (repeat)';

sub top(Int $f) { $f }
my $t = True;
is top($t) + top($t), 2, 'top-level sub, variable argument twice';
