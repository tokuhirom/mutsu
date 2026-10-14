use Test;

plan 6;

nok "zabc" ~~ /z ^ abc/, '^ after an atom never matches';
nok "def\nabc" ~~ /def \n ^ abc/, '^ after \n is start of string, not of line';
ok "abc" ~~ /[ ^ ] abc/, '^ inside a group at position 0 matches';
ok "abc" ~~ /<?> ^ abc/, '^ after a zero-width atom at position 0 matches';
ok "abc" ~~ /^ abc/, 'leading ^ still anchors';
ok "ab" ~~ /a | ^ b/, 'alternation with ^ branch';

done-testing;
