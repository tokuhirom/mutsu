use Test;
plan 5;

# `$¢` is the match cursor: Nil outside a match like `$/`, and the current
# match state inside a regex code block (Type/Match.rakudoc; values compared
# with rakudo 2026.09).
is-deeply $¢, Nil, '$¢ is Nil before any match';
is $¢.^name, 'Nil', '... and its type is Nil, not Any';

'ab' ~~ /a/;
is-deeply $¢, Nil, '$¢ stays Nil after a match outside the regex';

my $c;
'ab' ~~ /a { $c = $¢ } b/;
ok $c.defined, '$¢ inside a regex code block is the match state';

is "¢ → {$¢.raku}/ is $/", '¢ → Nil/ is ab', 'interpolated outside the regex';
