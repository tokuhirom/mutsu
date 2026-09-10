use Test;

# An `is default(...)` container declared in EXPRESSION position kept its
# default only while no other block in the file declared the same name. The
# top-level `locals` list is shared by every bare block at the same level, so a
# by-name slot search from one block reached a sibling block's slot -- still
# uninitialized -- and the default was tagged onto that instead of onto the
# container the expression returns.

plan 8;

# The repro: two disjoint blocks, neither running before the other's compile.
{
    my $z = (my %u is default(42));
    is $z<nope>, 42, 'a hash default survives a same-named sibling declaration';
}
{
    my %u;
    is %u.elems, 0, 'and the sibling hash is unaffected';
}

# Reversed order: the interaction is compile-time, so both orders must hold.
{
    my %v;
    is %v.elems, 0, 'the sibling hash comes first here';
}
{
    my $z = (my %v is default(7));
    is $z<nope>, 7, 'a hash default survives a sibling declared EARLIER';
}

# The same for the `@` sigil.
{
    my $z = (my @a is default(9));
    is $z[5], 9, 'an array default survives a same-named sibling declaration';
}
{
    my @a;
    is @a.elems, 0, 'and the sibling array is unaffected';
}

# A container-replacing trait was never affected and must stay that way.
{
    my $z = (my %w is SetHash);
    is $z.^name, 'SetHash', 'a container trait still replaces the container';
}
{
    my %w;
    is %w.elems, 0, 'and its sibling is unaffected too';
}
