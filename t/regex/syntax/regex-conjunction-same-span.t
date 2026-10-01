use Test;

# Every branch of a `&` conjunction must match the same span. The
# position-only matcher behind `.comb` took the longest branch end instead, so
# `"ab cd".comb(/ \w+ & <[a..c]>+ /)` found `cd`, where raku finds only `c`.
# Expected values are raku's.

plan 4;

is "ab cd".comb(/ \w+ & <[a..c]>+ /).join("|"), 'ab|c', '.comb keeps only the common span';
is "abc xbz".comb(/ <[a..c]>+ & \w\w /).join("|"), 'ab', '.comb with a fixed-width branch';
is "ab cd".match(/ \w+ & <[a..c]>+ /, :g).join("|"), 'ab|c', '.match(:g) agrees';
is ~("ab12" ~~ / (\w+) & (\w\w) /), 'ab', 'a capturing conjunction matches the common span';
