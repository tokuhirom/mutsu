use v6;
use Test;

# `ATOM+ %% SEP` means "one or more ATOM, separated by SEP" = `ATOM (SEP ATOM)*`,
# so each ATOM is matched a single item at a time with SEP required between
# items. mutsu's string-based LTM expansion double-quantified a simple atom that
# carried its own trailing quantifier (`\w+ %% X` expanded to `\w+ (X \w+)*`),
# letting each item greedily eat a whole run — so `\w+ %% 'X'` wrongly matched
# `"abc"` (should stop at `"a"`).

plan 12;

# --- the separator must appear BETWEEN items ------------------------------
is ~($_ given ('abc' ~~ / ^ \w+ %% 'X' /)), 'a',
    '\w+ %% X stops at the first item when no separator follows';
is ~($_ given ('aaa' ~~ / ^ a+ %% '.' /)), 'a',
    'a+ %% . stops at the first item';
is ~($_ given ('abc' ~~ / ^ .+ %% 'X' /)), 'a',
    '.+ %% X stops at the first item';
ok !('abc' ~~ / ^ \w+ %% 'X' $ /),
    '\w+ %% X anchored does not match a run with no separators';

# --- but a properly separated run matches fully ---------------------------
ok ('aXbXc' ~~ / ^ \w+ %% 'X' $ /), '\w+ %% X matches a separated run';
is ~$/, 'aXbXc', '... and consumes the whole separated run';
ok ('a.b.c' ~~ / ^ \w+ %% '.' $ /), '\w+ %% . matches a dotted run';

# --- `*` and `?` quantifier variants --------------------------------------
is ~($_ given ('abc' ~~ / ^ \w* %% 'X' /)), 'a',
    '\w* %% X stops at the first item';
ok ('' ~~ / ^ \w* %% 'X' $ /), '\w* %% X matches the empty string';

# --- regression guards: existing separator forms still work ---------------
ok ('1.2.3.4' ~~ / ^ (\d) ** 4 % '.' $ /), '(\d) ** 4 % "." still matches 1.2.3.4';
ok ('1,2,3' ~~ / ^ \d+ % ',' $ /), '\d+ % "," still matches 1,2,3';
ok ('a-b-c' ~~ / ^ \w+ %% '-' $ /), '\w+ %% "-" still matches a-b-c';
