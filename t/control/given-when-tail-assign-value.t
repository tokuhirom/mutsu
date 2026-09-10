use Test;

# The value of a `given`/`when` block whose final statement is an assignment
# (`$x = ...`) or bind (`$x := ...`) is that assigned value, not Nil. A bare
# `given`/`when` in tail position (e.g. as a sub/method return, or the `else`
# branch of a `with`, which desugars to a `given`) used to drop the assignment's
# value and yield Nil. Surfaced by a two-phase `pull-one { with $!k {...} else
# { $!k := ... } }` iterator whose `else` value was lost.

plan 8;

# given tail, plain assign
sub g-assign { my $y; given 3 { $y = 5 } }
is g-assign(), 5, 'given tail plain assign is the block value';

# given tail, bind
sub g-bind { my $y; given 3 { $y := 5 } }
is g-bind(), 5, 'given tail bind is the block value';

# when tail, assign
sub w-assign { my $y; given 3 { when 3 { $y = 5 } } }
is w-assign(), 5, 'when tail assign is the block value';

# with/else (desugars to given), assign in else
sub with-else-assign { my $y; with Any { 1 } else { $y = 9 } }
is with-else-assign(), 9, 'with/else tail assign is the block value';

# with/else, bind on a private attribute (the KV-iterator shape)
class C {
    has $!k;
    method f {
        with $!k { my $v = $!k; $!k := Mu; "val$v" }
        else     { $!k := 7 }
    }
}
my $c = C.new;
is $c.f, 7,      'with/else tail bind on private attr returns the bound value';
is $c.f, 'val7', 'the bind persisted (next call sees it via with)';

# do given still works (regression guard)
my $z;
is (do given 4 { $z = 8 }), 8, 'do given tail assign yields the value';

# a plain literal tail still yields the literal (regression guard)
sub g-lit { given 3 { 42 } }
is g-lit(), 42, 'given tail literal still yields the literal';
