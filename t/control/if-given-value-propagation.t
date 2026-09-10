use Test;

# A control-flow block (`given`) in branch-final position of an `if`/`with`
# must propagate its value outward, just like in a `do {}` block. Previously
# `if $c { given $v { ... } }` and statement-form `with $v { ... }` (which the
# parser lowers to `if defined($v) { given $v { ... } }`) returned Nil.

plan 8;

sub with_var { my $v = 5; with $v { "got-$_" } }
is with_var(), 'got-5', 'with $var propagates block value';

sub with_var_lit { my $v = 5; with $v { 99 } }
is with_var_lit(), 99, 'with $var propagates literal body value';

sub with_lit { with 5 { "lit-$_" } }
is with_lit(), 'lit-5', 'with literal still works';

sub if_given { my $v = 5; if $v { given $v { "g-$_" } } }
is if_given(), 'g-5', 'if { given } propagates inner given value';

sub if_given_lit { my $v = 5; if $v { given $v { 42 } } }
is if_given_lit(), 42, 'if { given } propagates literal value';

sub given_given { my $v = 5; given $v { given $v { "gg-$_" } } }
is given_given(), 'gg-5', 'given { given } still works';

sub if_lit { my $v = 5; if $v { "if-$v" } }
is if_lit(), 'if-5', 'plain if value still works';

# else-branch given should also propagate
sub else_given { my $v = 5; if False { 1 } else { given $v { "e-$_" } } }
is else_given(), 'e-5', 'else { given } propagates value';
