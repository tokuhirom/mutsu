use Test;

plan 8;

# `<( … )>` narrows a match to the marked region even though the pattern
# consumed more. `.subst` spans its replacement with the match, so ignoring the
# markers overwrote the context the pattern had only looked at.

is 'xaby'.subst(/a <( b )>/, 'Z'), 'xaZy',
    'subst replaces only the marked region, not the consumed one';
is 'a-w'.subst(/. <( '-' )> ./, 'X'), 'aXw',
    'markers on both sides keep both context characters';
is 'a-w'.subst(/. <( '-' )> ./, '..', :g), 'a..w',
    ':g subst honours the markers too';
is 'a-z-'.subst(/. <( '-' )> ./, '..', :g), 'a..z-',
    'a trailing character with no follower is left alone';

# The `s///` operator form goes the same way.
{
    my $s = 'a-w';
    $s ~~ s/. <( '-' )> ./X/;
    is $s, 'aXw', 's/// honours the markers';
}

# A marker on one side only.
is 'foobar'.subst(/foo <( bar )>/, 'X'), 'fooX',
    'a leading-only marker keeps the prefix';
is 'foobar'.subst(/<( foo )> bar/, 'X'), 'Xbar',
    'a trailing-only marker keeps the suffix';

# Without markers nothing changes.
is 'a-w'.subst(/'-'/, 'X'), 'aXw',
    'a pattern with no markers still replaces its whole match';
