use v6;
use Test;

# HTML::Component::Encode writes `<[\xA0..\xD8FF \xE000..\xFFFD]>{}`: a class
# range whose endpoint is a surrogate. No string holds a surrogate, so such a
# range ends at U+D7FF (or starts at U+E000). The unit must also keep a regex
# source tree, or the RakuAST frontend refuses the whole unit.

plan 12;

ok "a\xA1" ~~ /<[\xA0..\xD8FF]>/, 'range ending in a surrogate matches inside it';
ok "\x[D7FF]" ~~ /<[\xA0..\xD8FF]>/, 'the last scalar value below the surrogates is in';
nok "ab" ~~ /<[\xA0..\xD8FF]>/, 'below the range is out';
nok "\x[E000]" ~~ /<[\xA0..\xD8FF]>/, 'above the surrogates is out';
ok "\x[E001]" ~~ /<[\xA0..\xD8FF \xE000..\xFFFD]>/, 'second range of the list matches';
nok "\x[10000]" ~~ /<[\xA0..\xD8FF \xE000..\xFFFD]>/, 'past the second range is out';
ok "\x[E000]" ~~ /<[\xD800..\xE000]>/, 'range starting at a surrogate begins at U+E000';
ok "a\xA1" ~~ /<[\xA0..\x[D8FF]]>/, 'bracketed escape endpoint';

my $r = /<[\xA0..\xD8FF]>{}/;
ok "x\xA1" ~~ $r, 'the regex value with a code block matches';
is $/.ord, 161, 'the code block ran';

is "x\xA1y".trans(/<[\xA0..\xD8FF]>{}/ => { '&#' ~ $/.ord ~ ';' }), 'x&#161;y',
    'trans with the class and a closure';
is (EVAL q[my $q = /<[\xA0..\xD8FF]>/; so "\xA1" ~~ $q]), True, 'inside EVAL';

done-testing;
