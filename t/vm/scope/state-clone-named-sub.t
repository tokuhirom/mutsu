use v6;
use nqp;
use Test;

# A closure clone of a named sub (`&f.clone`, `&f but R`, which is
# `.clone.^mixin`) gets its own `state` variables, as on rakudo; the
# representation copy `nqp::clone` keeps sharing the routine's. mutsu ran a
# named routine's clone in the routine's registration scope (#18165).

plan 8;

my role R { }
sub f { state $k = 0; $k++ }
f(); f();

my $mixed = &f but R;
is $mixed(), 0, 'a `but` copy starts its own state';
is &f.clone()(), 0, 'a .clone starts its own state';
is f(), 2, 'the routine keeps its own state';
is $mixed(), 1, 'the copy keeps counting on its own';

my $c = &f.clone;
is-deeply ($c(), $c()), (0, 1), 'a clone counts on its own across calls';
is f(), 3, 'unaffected by the clone';

sub h { state $n = 0; $n++ }
h();
my $repr = nqp::clone(&h);
is $repr(), 1, 'nqp::clone shares the routine state';
is h(), 2, 'and the routine sees the copy\'s update';
