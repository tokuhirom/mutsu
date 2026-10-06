use Test;

plan 2;

ok "ab" ~~ / $( rx/ <["']>? a / ) b /,
    'quotes in a nested rx// character class do not close the contextualizer';
ok "ab" ~~ rx/ <["']>? a /,
    'the nested rx// still matches the character class and following literal';
