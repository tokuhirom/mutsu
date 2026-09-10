use v6;
use Test;

# A `[...]`-group atom with a trailing quantifier and a `%`/`%%` separator must
# be quantified ONCE per separated item — `[.]+ %% X` = `[.] (X [.])*`. mutsu's
# LTM string-expansion only stripped the quantifier off angle-bracket subrule
# atoms (`<foo>+`), so a square-bracket group fell through and was
# double-quantified (`[.]+ (X [.]+)*`), letting each item eat a greedy run.

plan 9;

is ~($_ given ('abc' ~~ / [.]+ %% 'X' /)), 'a',
    '[.]+ %% X stops at the first item (no separator follows)';
is ~($_ given ('abc' ~~ / [\w]+ %% 'X' /)), 'a',
    '[\w]+ %% X stops at the first item';
is ~($_ given ('abc' ~~ / [ \w ]* %% 'X' /)), 'a',
    '[ \w ]* %% X stops at the first item';

# A properly separated run still matches whole.
is ~($_ given ('a-b-c' ~~ / [\w]+ %% '-' /)), 'a-b-c',
    '[\w]+ %% "-" matches a separated run';

# The item group itself may contain a quantifier / a frugal quantifier.
is ~($_ given ('ab,ab' ~~ / [ab]+ %% ',' /)), 'ab,ab',
    '[ab]+ %% "," treats each "ab" run as one item';
is ~($_ given ('0.0.1' ~~ / [[ . ]+?]* %% [ '\\' . ]+ /)), '0',
    'frugal-nested group [[.]+?]* %% [\\ .]+ takes a single frugal item';

# Nested groups with the separator, fully separated.
is ~($_ given ('12-34-56' ~~ / [\d ** 2]+ %% '-' /)), '12-34-56',
    '[\d ** 2]+ %% "-" matches two-digit groups separated by "-"';

# Regression guards: angle-bracket subrule atoms and simple atoms still work.
grammar G { token v { <[a..z]> } }
is ~($_ given ('abc' ~~ / <G::v>+ %% 'X' /)), 'a',
    '<subrule>+ %% X still stops at the first item';
is ~($_ given ('a-b-c' ~~ / <[a..z]>+ %% '-' /)), 'a-b-c',
    '<[a..z]>+ %% "-" still matches a separated run';
