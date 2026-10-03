use v6;
use Test;

plan 4;

# `｢...｣` in a regex is the raw quote form: it has no escapes, so `｢\\｣`
# matches two backslashes (TOML::Thumb's `｢\\｣ { make "\\" }` escape rule).

nok "\\" ~~ / ^ ｢\\｣ $ /, '｢\\｣ does not match one backslash';
ok "\\\\" ~~ / ^ ｢\\｣ $ /, '｢\\｣ matches two backslashes';
ok 'a\nb' ~~ / ｢\n｣ /, '｢\n｣ is backslash then n';
ok "it's" ~~ / ｢it's｣ /, 'a quote inside is literal';
