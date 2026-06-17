use Test;

plan 4;

# A failed symbolic lookup via `::(...)` produces an X::NoSuchSymbol failure
# (Raku semantics), not a bare X::AdHoc.

throws-like { ::('') }, X::NoSuchSymbol,
    'empty symbolic lookup ::(\'\') throws X::NoSuchSymbol';
throws-like { ::('NoSuchSymbolNameXYZ') }, X::NoSuchSymbol,
    'unknown symbolic lookup throws X::NoSuchSymbol';

{
    my $err;
    try { ::(''); CATCH { default { $err = $_ } } }
    is $err.symbol, '', 'the .symbol attribute carries the looked-up name';
}

# A successful symbolic lookup still resolves normally.
{
    my class Foo { }
    ok ::('Foo') === Foo, 'a declared type still resolves via ::(name)';
}
