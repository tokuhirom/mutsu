use Test;
plan 13;

ok "abc" ~~ /abc/, 'literal match';
ok "abc" ~~ /b/, 'substring match';
nok "abc" ~~ /^b/, 'anchor start fails';
ok "abc" ~~ /^a/, 'anchor start matches';
ok "abc" ~~ /c$/, 'anchor end matches';
ok "abc" ~~ /a.c/, 'dot matches any char';
ok "aaa" ~~ /a+/, 'plus quantifier';
ok "" ~~ /a*/, 'star quantifier allows empty';
ok "123" ~~ /\d+/, 'digit class';
ok "a1_" ~~ /\w+/, 'word class';
ok "a b" ~~ /\s/, 'space class';
ok "abc".match(/b/), '.match uses regex';
ok "abc" ~~ rx/ab/, 'rx// literal match';
