use Test;

# Sigils inside a character class are literal members: `<[:@!$&]>` matches the
# characters `:`, `@`, `!`, `$` and `&`, and never interpolates an `@!attr` or
# a `$var`. Such a pattern is static, so it is parsed once and reused; a match
# repeated many times (as a grammar token is) must keep answering the same.

plan 9;

grammar G {
    token TOP { <pchars> }
    token pchars { <[A..Za..z0..9._~:@!$&'()*+,;=-]>+ }
}

ok G.parse('a@b!c$d&e'), 'sigil members of a class match as literals';
nok G.parse('a%b'), 'a character outside the class does not match';
is G.parse('x:@!y')<pchars>.Str, 'x:@!y', 'the whole run is captured';

my @ok = (^200).map({ G.parse("p@!q$_") ?? 1 !! 0 });
is @ok.sum, 200, 'repeated parses keep matching';

my $var = 'zzz';
ok 'b$var' ~~ /^ <[b$var]>+ $/, '`$var` inside a class is the characters b, $, v, a, r';
nok 'zzz' ~~ /^ <[b$var]>+ $/, 'and not the value of $var';
ok '@!x' ~~ /^ <-[abc]>+ $/, 'a negated class still matches';
ok 'a' ~~ /<?[@!a]> ./, 'a lookahead class with sigil members';
is ('q@!' ~~ /<[@!]>+/).Str, '@!', 'only the sigil members are matched';
