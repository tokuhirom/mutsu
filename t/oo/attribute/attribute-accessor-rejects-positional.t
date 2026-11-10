use Test;

# Found with Template::Nest::XS t/00-basic: `$nest."$method"(2)` on a generated
# accessor must die (raku: "Too many positionals"), not assign.
plan 9;

class A { has Bool $.f is rw = False; has $.g is rw = 1; has $.h = 2 }
my $a = A.new;

throws-like { $a."f"(2) }, X::AdHoc, :message(/'Too many positionals'/), 'rw accessor with a positional dies';
throws-like { $a.g(2) }, X::AdHoc, 'rw scalar accessor with a positional dies';
throws-like { $a.h(2) }, X::AdHoc, 'ro accessor with a positional dies';
is $a.g, 1, 'the failed call did not assign';
lives-ok { $a.g(:x) }, 'a named argument is ignored';

$a.f = True;
is $a.f, True, 'assignment through the accessor';
$a."f"() = False;
is $a.f, False, 'assignment through a dynamic method name';

class B { has @.xs is rw; has %.h is rw }
my $b = B.new;
$b.xs = 1, 2;
$b.h = a => 1;
is-deeply $b.xs, [1, 2], 'array attribute assignment';
is-deeply $b.h, {a => 1}, 'hash attribute assignment';
