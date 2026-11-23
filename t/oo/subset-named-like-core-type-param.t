use Test;

# A subset named like a core type (`subset Int of Any where ...`) shadows the
# core type in a parameter too: the light call path's registration-time check
# must not admit an Int argument by its tag alone (#19031).

plan 7;

subset Int of Any where * > 10;

sub t(Int $x) { "ok $x" }
throws-like { t(5) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected Int but got Int (5)",
    'a sub parameter runs the where';
is t(50), 'ok 50', '... and binds what it accepts';

sub u(Int:D $x) { "ok $x" }
throws-like { u(5) }, X::TypeCheck::Binding::Parameter, 'a :D parameter runs the where';
is u(50), 'ok 50', '... and binds what it accepts';

# Call it often enough to settle on any cached fast path.
my $hits = 0;
for ^50 { $hits++ if (try t($_)).defined }
is $hits, 39, 'repeated calls keep running the where';

# The where is all that decides: a Str the subset accepts binds.
subset Str of Any where { .chars > 1 };
sub s(Str $x) { "s $x" }
is s(42), 's 42', 'a Str-named subset admits a non-Str its where accepts';
throws-like { s("a") }, X::TypeCheck::Binding::Parameter, 'and rejects a Str its where refuses';
