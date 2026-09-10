use v6;
use Test;

plan 8;

# (1) `%hash.method()` must interpolate in qq strings, like `@array.method()`.
{
    my %h = a => 42, b => 666;
    my $s = "keys = %h.keys().sort()";
    is $s, "keys = a b", '%h.method() interpolates in a qq string';
}

{
    my %h = x => 1, y => 2, z => 3;
    is "n = %h.elems()", "n = 3", '%h.elems() interpolates';
}

# A bare `%h` (no postcircumfix / method) stays literal.
{
    my %h = a => 1;
    is "raw = %h", "raw = %h", 'bare %h without accessor stays literal';
}

# (2) A type object whose class defines a user `.Str` stringifies via it in
# interpolation (both `$var` and `{ ... }` forms), matching infix `~`.
{
    class A {
        method Str  { "foo" }
        method gist { "bar" }
    }
    my $t = A;
    is "v = $t",   "v = foo", 'type object interpolates via user .Str ($var form)';
    is "v = { A }", "v = foo", 'type object interpolates via user .Str ({ } form)';
    is "v = " ~ A, "v = foo", 'infix ~ agrees (unchanged)';
}

# A user `.Stringy` takes precedence over `.Str`, as with infix ~.
{
    class C {
        method Stringy { "stringy" }
        method Str     { "str" }
    }
    is "c = { C }", "c = stringy", 'user .Stringy wins over .Str in interpolation';
}

# A plain type object without a user stringifier stays empty (no crash).
{
    class D { }
    is "d = { D }|", "d = |", 'plain type object interpolates to empty';
}
