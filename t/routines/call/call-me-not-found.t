use Test;

# Postfix `()` on a non-Callable value resolves to a CALL-ME method, which the
# basic types do not provide, so invoking them throws X::Method::NotFound with
# method 'CALL-ME' and the value's typename.

plan 4;

throws-like 'sub (int $i) { $i() }(42)', X::Method::NotFound;

throws-like 'my $i = 42; $i()', X::Method::NotFound,
    method => 'CALL-ME', typename => 'Int';

throws-like 'my $s = "x"; $s()', X::Method::NotFound,
    method => 'CALL-ME', typename => 'Str';

# A real Callable still invokes normally.
my $c = -> $x { $x + 1 };
is $c(2), 3, 'invoking a Callable value still works';
