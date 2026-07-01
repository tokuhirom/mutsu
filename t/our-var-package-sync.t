use Test;

plan 6;

# Inside a package, `our $b` links the lexical alias `$b` to the package variable
# `Foo::b`; a package-qualified store must be visible immediately through `$b`.

{
    package Foo {
        our $b;
        $Foo::b = 42;
        is $b, 42, 'direct $Foo::b = 42 is visible through the our alias $b';
    }
}

{
    package Bar {
        our $b;
        $::('Bar::b') = 99;
        is $b, 99, 'symbolic $::(...) store is visible through the our alias';
    }
}

# A `$` symbolic-deref store is a scalar assignment: it stores the whole list
# (itemized), not just the first element, and its rvalue is itemized.
{
    sub l () { 1, 2 }
    package Baz {
        our $b;
        my @z = (flat $::('Baz::b') = l(), l());
        is $b.elems, 2, 'the whole list (1,2) is stored, not just the first item';
        is @z.elems, 3, 'the assignment rvalue is itemized so flat keeps (1,2)';
        is @z[0].elems, 2, '@z[0] is the itemized (1,2)';
    }
}

# Top-level our still works.
{
    our $top = 7;
    is $GLOBAL::top // $top, 7, 'top-level our variable reads back';
}
