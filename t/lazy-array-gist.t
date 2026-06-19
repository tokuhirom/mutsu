use Test;

# A lazy (infinite-backed) array must render a bounded placeholder instead of
# materializing (and dumping) its capped backing store.

plan 12;

# my @a = 1..* keeps the array lazy
{
    my @a = 1..*;
    ok @a.is-lazy, '@a = 1..* is lazy';
    is @a.gist, '[...]', 'lazy array .gist is [...]';
    is @a.raku, '[...]', 'lazy array .raku is [...]';
    is @a.Str, '...', 'lazy array .Str is ...';
}

# say / put / print render via gist / Str
{
    my @a = 1..*;
    is-deeply (try { @a.gist }), '[...]', 'gist does not hang or dump';
    my $interp = "@a[]";
    is $interp, '...', 'interpolated lazy array is ...';
}

# nested lazy array inside a normal array renders [...] for the inner one
{
    my @a = 1..*;
    my @b = 5, @a;
    is @b.gist, '[5 [...]]', 'nested lazy array gist';
}

# a normal (finite) array is unaffected
{
    my @n = 1, 2, 3;
    is @n.gist, '[1 2 3]', 'finite array gist unchanged';
    is @n.Str, '1 2 3', 'finite array Str unchanged';
    is @n.raku, '[1, 2, 3]', 'finite array raku unchanged';
    my @r = 1..10;
    is @r.gist, '[1 2 3 4 5 6 7 8 9 10]', 'finite range materializes';
    is @r.elems, 10, 'finite range elems';
}
