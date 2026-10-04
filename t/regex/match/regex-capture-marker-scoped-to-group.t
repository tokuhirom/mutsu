use Test;

plan 12;

# `)>` inside a capturing group used to be read as the group's `)` followed by
# a stray `>` ("Unrecognized regex metacharacter >"), and `<(` inside one
# unbalanced the group scanner (#11570). A capture group is a Match of its
# own, so `<(` / `)>` inside it narrow the GROUP's capture, not the whole
# match -- as in rakudo.

my $m = 'xab' ~~ /(a )> b)/;
is $m.Str, 'ab', '`)>` inside a group: the whole match is unaffected';
is $m[0].Str, 'a', '... and the group\'s capture ends at the marker';
is $m.from, 1, '... from where the match started';

$m = 'xaby' ~~ /x (a <( b )> ) y/;
is $m.Str, 'xaby', '`<( … )>` inside a group leaves the whole match alone';
is $m[0].Str, 'b', '... and narrows the group';

$m = 'xab' ~~ /$<x>=(a )> b)/;
is $m<x>.Str, 'a', 'a named capture group is narrowed too';

$m = 'abab' ~~ /(a <( b)+/;
is $m[0].map(*.Str).join(','), 'b,b', 'each iteration of a quantified group is narrowed';

$m = 'aab' ~~ /((a) <( a b)/;
is-deeply ($m.Str, $m[0].Str, $m[0][0].Str), ('aab', 'ab', 'a'),
    'a nested group keeps its own span';

# Outside a capture group the markers still set the whole match.
is ('xaby' ~~ /x <( a b )> y/).Str, 'ab', 'top-level markers narrow the match';
is ('xab' ~~ /[(a )> b)]/)[0].Str, 'a', 'a non-capturing group is not a scope';

# Escaped, they are plain characters.
is ('a)>' ~~ /a \)\>/).Str, 'a)>', 'an escaped `)>` matches literally';
is ('a>' ~~ /(a \>)/).Str, 'a>', 'an escaped `>` inside a group';
