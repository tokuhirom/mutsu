use Test;

# Where sigspace puts whitespace around the `~` goal construct in a `rule`
# (#11234): the space before `~` allows whitespace after the opener, the space
# after the inner atom allows it before the closer, and the space after the
# goal atom allows it after the closer. Expected values are Rakudo's.

grammar T0001 { rule TOP {'['~']'<k> 'z' }; token k { <[a..z]>+ } }
grammar T0010 { rule TOP {'['~']' <k>'z' }; token k { <[a..z]>+ } }
grammar T1000 { rule TOP {'[' ~']'<k>'z' }; token k { <[a..z]>+ } }
grammar T0100 { rule TOP {'['~ ']'<k>'z' }; token k { <[a..z]>+ } }
grammar TAll  { rule TOP { '[' ~ ']' <k> 'z' }; token k { <[a..z]>+ } }

my @s = '[a]z', '[ a]z', '[a ]z', '[a] z', '[ a ] z', '[a ] z', '[ a ]z';

sub row($g) { @s.map({ $g.parse($_) ?? 1 !! 0 }).join }

is row(T0001), '1010000', 'space after the inner atom: whitespace before the closer only';
is row(T0010), '1001000', 'space after the goal atom: whitespace after the closer only';
is row(T1000), '1100000', 'space before ~: whitespace after the opener only';
is row(T0100), '1000000', 'space between ~ and the goal means nothing';
is row(TAll),  '1111111', 'all spaces: whitespace everywhere';

grammar P { rule TOP { '('~')' <k>+ % ',' 'z' }; token k { <[a..z]> } }
ok  P.parse('(a,b ) z'), 'quantified inner atom keeps its trailing whitespace inside';
nok P.parse('( a,b) z'), 'no space before ~: no whitespace after the opener';

done-testing;
