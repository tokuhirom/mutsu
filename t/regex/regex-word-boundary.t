use Test;

plan 16;

# << left word boundary
ok "abc-def" ~~ /<<def/, '<< matches left word boundary';
ok "abc def" ~~ /<<def/, '<< matches left word boundary with space';
ok "abc" ~~ /<<abc/, '<< matches at start of string';
nok "abcdef" ~~ /<<def/, '<< does not match mid-word';

# >> right word boundary
ok "abc-def" ~~ /abc>>/, '>> matches right word boundary';
ok "abc def" ~~ /abc>>/, '>> matches right word boundary with space';
ok "abc" ~~ /abc>>/, '>> matches at end of string';
nok "abcdef" ~~ /abc>>/, '>> does not match mid-word';

# << and >> combined
ok "abc-def" ~~ /<<def>>/, '<< and >> combined';
ok "hello world" ~~ /<<hello>>/, '<< and >> whole word';
nok "helloworld" ~~ /<<hello>>/, '<< and >> do not match part of word';

# guillemet word boundaries (use << >> form since guillemet chars may not render)
ok "hello world" ~~ /<< hello >>/, 'word boundary with spaces';
ok "test" ~~ /<< test >>/, 'word boundary around whole string';

# <.alpha> dot-prefixed named regex
ok "hello" ~~ /<.alpha>+/, '<.alpha>+ matches letters';
ok "123" ~~ /<.digit>+/, '<.digit>+ matches digits';
nok "123" ~~ /<.alpha>+/, '<.alpha>+ does not match digits';
