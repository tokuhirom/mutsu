# `$_ = Nil` inside `given $x` / `for $x` assigns the topic's SOURCE container, so the Nil decays to
# that variable's `is default(...)` (Jupyter::Kernel::Sandbox's `given $output { $_ = Nil if ... }`)
# (#16585).
use Test;
plan 7;

my $a is default(Nil) = 5;
given $a { $_ = Nil }
is-deeply $a, Nil, 'given: is default(Nil) stays Nil';

my $b is default(42) = 5;
given $b { $_ = Nil }
is $b, 42, 'given: is default(42) becomes 42';

my $c is default(Nil) = 5;
for $c { $_ = Nil }
is-deeply $c, Nil, 'for: is default(Nil) stays Nil';

my $d = 5;
given $d { $_ = Nil }
is-deeply $d, Any, 'a plain scalar decays to Any';

my $e is default(Nil) = True but role { method hide { True } };
given $e { $_ = Nil if .?hide; $_ = Nil if $_ === Any }
is-deeply $e, Nil, 'the sandbox idiom';

my $f is default(Nil);
$f = Nil;
is-deeply $f, Nil, 'direct assignment (always worked)';

my $g is default(Nil) = 7;
given $g { $_ = 8 }
is $g, 8, 'a non-Nil assignment is untouched';
