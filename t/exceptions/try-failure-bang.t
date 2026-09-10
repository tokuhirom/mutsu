use Test;

plan 10;

# A `try` block whose result is an *unthrown* Failure sets $! to that
# Failure's exception (Raku semantics), not Nil.
{
    my $r = try { "42abc".Int };
    nok $r.defined, 'try over failing .Int yields an undefined (Failure) result';
    is $!.^name, 'X::Str::Numeric', '$! is set to the Failure exception after try';
}

# An explicit Failure returned from a try block also populates $!.
{
    my $r = try { Failure.new("custom") };
    nok $r.defined, 'explicit Failure from try is undefined';
    is $!.^name, 'X::AdHoc', '$! is the explicit Failure exception';
}

# A successful try with a normal value resets $! to Nil/Any.
{
    my $r = try { 41 + 1 };
    is $r, 42, 'successful try returns its value';
    nok $!.defined, '$! is undefined after a successful try';
}

# A thrown exception inside try still sets $! (unchanged behaviour).
{
    my $r = try { die "boom" };
    nok $r.defined, 'try over die yields undefined';
    is $!.^name, 'X::AdHoc', '$! is X::AdHoc after a caught die';
}

# $! reflects the *most recent* try.
{
    try { "xx".Int };
    my $first = $!.^name;
    try { 1 + 1 };
    is $first, 'X::Str::Numeric', 'first try set $! to the numeric failure';
    nok $!.defined, 'second (successful) try cleared $!';
}
