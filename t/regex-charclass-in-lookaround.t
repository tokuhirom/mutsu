use v6;
use Test;

# A character class nested inside a lookaround assertion must not desync the
# regex scanner or the lookaround-body parser: its content is literal, so a
# quote character in it (`<-["]>`) must not open a bogus string that swallows
# the rest of the regex. Discovered vendoring Slang::Tuxic (ADR-0026), whose
# methodop token contains `<!before '"' <-["]>*? [\s|$] >`.

plan 10;

ok "ab" ~~ / a <?before <-["]> > b /, 'char class in lookahead matches';
nok 'a"' ~~ / a <?before <-["]> > /, 'char class in lookahead rejects quote char';
ok 'a"b' ~~ / a <!before '"' <-["]>*? '"' > /,
    'quoted literal + char class + quoted literal in negative lookahead';
ok "axxb" ~~ / a <?before <-["]>*? b > /, 'lazy-quantified char class in lookahead';

# The Slang::Tuxic methodop shape: alternation, code assertions, and a
# char class with a quote — all inside one group.
my $r = rx/ [ | <?['"]> [ <!{$*QSIGIL}> || <!before '"' <-["]>*? [\s|$] > ] ] /;
ok '"x' ~~ $r, 'Tuxic-shaped regex matches a double quote';
ok "'x" ~~ $r, 'Tuxic-shaped regex matches a single quote';
nok "x" ~~ $r, 'Tuxic-shaped regex rejects a non-quote';

# The same shape inside a token body of a role must parse (the role is data
# until composed; parsing the file must not blow up on the token body).
my role TuxicShape {
    token m {
        [
          | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <-["]>*? [\s|$] > ]
            <quote>
        ] \s* <.unspace>?
    }
}
ok TuxicShape.^name eq 'TuxicShape', 'role with Tuxic-shaped token body parses';

# Compound char class inside a lookaround.
ok "a5" ~~ / a <?before <[0..9]+[x]> > /, 'compound char class in lookahead';
nok "ay" ~~ / a <?before <[0..9]+[x]> > /, 'compound char class in lookahead rejects';
