use Test;

plan 10;

# ::<$var> syntax - pseudo-stash lookup with angle brackets
{
    my $x = 42;
    is ::<$x>, 42, '::<$var> looks up variable in current scope';
}

# ::{'$var'} syntax - pseudo-stash lookup with curly braces
{
    my $y = 99;
    is ::{'$y'}, 99, '::{"$var"} looks up variable in current scope';
}

# Bare :: as PseudoStash
{
    my $z = 7;
    ok ::.keys.elems > 0, 'bare :: returns a stash with entries';
}

# $Package::($expr)::suffix - indirect package variable interpolation
{
    $Terrain::Hill::mountain = 1024;
    my $mountain = 'Hill';
    is $Terrain::($mountain)::mountain, 1024,
        '$Package::($expr)::suffix indirect lookup works';
}

# $::($expr)::suffix - fully indirect package variable
{
    $Terrain::Hill::mountain = 1024;
    my $river = 'Terrain::Hill';
    is $::($river)::mountain, 1024,
        '$::($expr)::suffix fully indirect lookup works';
}

# ^shortname meta-method
{
    is Int.new.^shortname, 'Int', '^shortname on basic type';
    is Str.^shortname, 'Str', '^shortname on type object';
}

# Package-qualified ^shortname
{
    class Baz::Quux { };
    is Baz::Quux.^shortname, 'Quux', '^shortname strips package prefix';
}

# ::<$var> and ::{'$var'} inside function calls
{
    my $bear = 2.16;
    is(::{'$bear'}, 2.16, '::{"$var"} in function call');
    is(::<$bear>, 2.16, '::<$var> in function call');
}
