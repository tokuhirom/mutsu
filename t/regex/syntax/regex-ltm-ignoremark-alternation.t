use Test;

plan 5;

ok "äb" ~~ / [ x | :m a ] b /,
    ':m on a later alternation branch matches a precomposed subject';
ok "äb" ~~ / [ :m a | x ] b /,
    ':m on the first alternation branch matches a precomposed subject';
ok "a\x[301]b" ~~ / [ x | :m a ] b /,
    ':m on an alternation branch strips a combining mark from the subject';
ok "ab" ~~ / [ x | a ] b /,
    'ordinary literal alternation still matches';
ok "Ab" ~~ / [ x | :i a ] b /,
    'a branch-local case modifier survives literal alternation';
