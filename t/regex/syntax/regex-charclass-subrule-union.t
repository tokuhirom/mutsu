use v6;
use Test;

# A character class may union in a user-defined grammar token that matches a
# MULTI-character sequence: `<+name-sep>` where `token name-sep { '::' }`. mutsu's
# char class was single-char-only, so such a class never matched. This is the
# construct zef's `Zef::Identity` REQUIRE grammar uses for module names
# (`regex name { <-restricted +name-sep>+ }`).

plan 9;

grammar G {
    regex name       { <-restricted +name-sep>+ }
    token restricted { <[ : < > ( ) ]> }
    token name-sep   { '::' }
}

# The multi-char subrule matches inside the class.
ok G.subparse('::', :rule<name>).defined, 'bare <+name-sep> matches the "::" sequence';

# A qualified name flows across the `::` separator, stopping at a restricted char.
{
    my $m = G.subparse('Foo::Bar', :rule<name>);
    ok $m.defined, 'name matches across "::"';
    is ~$m, 'Foo::Bar', 'name captures the whole qualified identifier';
}

# The class stops at a restricted char (`:` alone is restricted, only `::` passes).
{
    my $m = G.subparse('Foo::Bar:ver', :rule<name>);
    is ~$m, 'Foo::Bar', 'name stops before a lone restricted ":"';
}

# The unioned subrule is matched SILENTLY (a char class never captures).
{
    my $m = G.subparse('Foo::Bar', :rule<name>);
    is $m.hash.keys.elems, 0, 'the unioned subrule does not leak a capture';
}

# A single-char user token union still works (regression guard).
grammar H {
    regex word    { <+letter +digit>+ }
    token letter  { <[a..z]> }
    token digit   { <[0..9]> }
}
{
    my $m = H.subparse('ab12', :rule<word>);
    is ~$m, 'ab12', 'single-char user-token union still matches';
}

# Purely-negated user token alone (regression guard: <-restricted> resolves the
# grammar token, not "match nothing").
grammar K {
    regex body       { <-restricted>+ }
    token restricted { <[ : ]> }
}
{
    my $m = K.subparse('abc:def', :rule<body>);
    is ~$m, 'abc', '<-restricted> negation resolves the grammar token';
}

# The subrule can be tried first (LTM) so a longer subrule match wins over a
# single class char at the same spot.
grammar L {
    regex t        { <+single +ab>+ }
    token single   { <[a..z]> }
    token ab       { 'ab' }
}
{
    # "ab" — LTM should let either interpretation match the full string.
    my $m = L.subparse('abc', :rule<t>);
    is ~$m, 'abc', 'LTM alternation over class + multi-char subrule matches greedily';
}

# The exact zef REQUIRE name shape.
grammar REQUIRE {
    regex name       { <-restricted +name-sep>+ }
    token restricted { [ ':' | '<' | '>' | '(' | ')' ] }
    token name-sep   { < :: > }
}
is ~REQUIRE.subparse('Acme::Foo::Bar', :rule<name>), 'Acme::Foo::Bar',
    'zef REQUIRE-style name matches a multi-part qualified name';
