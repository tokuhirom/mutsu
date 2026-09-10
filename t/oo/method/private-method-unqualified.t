use v6;
use Test;

plan 5;

# Calling a private method on something other than `self` without naming the
# defining package is X::Method::Private::Unqualified (Raku requires
# `$obj!Package::meth`). A qualified-but-untrusted call is the *different*
# X::Method::Private::Permission.

class A {
    method !secret { 42 }
    method reveal  { self!secret }   # self!meth is always allowed
}

# self!meth inside the class is fine.
is A.new.reveal, 42, 'self!private works inside the class';

# Unqualified external private call -> X::Method::Private::Unqualified.
{
    my $a = A.new;
    throws-like { $a!secret }, X::Method::Private::Unqualified,
        'unqualified external private call is X::Method::Private::Unqualified';
}

# Via EVAL too (matches the advent2011-day11 usage).
throws-like q[ class B { method !s { 1 } }; my $b = B.new; $b!s ],
    X::Method::Private::Unqualified,
    'unqualified external private call through EVAL';

# A qualified call to a class that does not trust the caller is Permission.
{
    my $a = A.new;
    throws-like { $a!A::secret }, X::Method::Private::Permission,
        'qualified untrusted private call is X::Method::Private::Permission';
}

# X::Method::Private::Unqualified carries the method name.
{
    my $a = A.new;
    my $ex;
    try { $a!secret; CATCH { default { $ex = $_ } } }
    is $ex.method, 'secret', 'the exception names the private method';
}
