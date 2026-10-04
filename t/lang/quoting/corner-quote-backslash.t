use Test;
plan 4;

ok "\\" ~~ / ^ ｢\｣ $ /, '｢\｣ matches one literal backslash';
ok "a\\b" ~~ / a ｢\｣ b /, '｢\｣ in the middle of a pattern';
ok "\\\\" ~~ / ^ ｢\\｣ $ /, '｢\\｣ matches two backslashes';
nok "x" ~~ / ^ ｢\｣ $ /, '｢\｣ does not match other text';
