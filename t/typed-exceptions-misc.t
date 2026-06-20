use Test;

# Typed exceptions covered by S32-exceptions/misc.t that mutsu previously
# threw as plain (untyped / attribute-less) runtime errors.

plan 7;

# X::Bind: binding into an immutable subscript target.
throws-like '(1,2)[0] := 3', X::Bind, 'bind into all-literal list element';
throws-like '10[0] := 1',   X::Bind, 'bind into a defined Int';
throws-like '"Hi"[0] := 1', X::Bind, 'bind into a defined Str';

# X::InvalidType: `hides` an unknown type (mirrors `does` and `returns`).
throws-like 'my class C hides Baz { }', X::InvalidType,
    typename => 'Baz', 'hides unknown type';

# X::Str::Numeric.source-indicator carries the offending source string.
throws-like 'use fatal; +("\b" x 10)', X::Str::Numeric,
    source-indicator => /'\b'/, 'prefix + on a bad string carries source-indicator';

# A bad numeric string still produces a *lazy* Failure (not an eager throw)
# without `use fatal`.
{
    my $f = +"abc";
    is $f.defined, False, 'prefix + on a bad string is an undefined Failure';
}

# Legitimate subscript binds still alias and write through.
{
    my @a;
    my $s = 5;
    @a[0] := $s;
    $s = 9;
    is @a[0], 9, 'legitimate @a[0] := $s aliases the source container';
}
