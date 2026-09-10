use v6;
use Test;

plan 6;

# `.comb(/regex/)` (which uses the non-capturing find-all matcher) must honour a
# separator quantifier whose atom carries a capture, e.g. `(\d) ** 4 % '.'`.
# Previously the find-all matcher ignored the `%` separator and treated the token
# as a plain count, so IPv4-style patterns matched nothing.

my $ips = "Go 127.0.0.1, I said! He went to 173.194.32.32.";

is-deeply $ips.comb(/ (\d ** 1..3) ** 4 % '.' /).List,
    ("127.0.0.1", "173.194.32.32"),
    '.comb with (capture) ** N % sep finds all IPv4 addresses';

is-deeply "1.2.3.4".comb(/ (\d) ** 4 % '.' /).List,
    ("1.2.3.4",),
    '.comb with single-digit captured groups and a separator';

# A non-capturing separated quantifier must keep working. `\d+ % '-'` is
# "`\d`, one or more, separated by `-`" = `\d (- \d)*`, so a bare run of digits
# ("44") matches only its first digit before the separator is required — matching
# Rakudo: `"1-2-3 44-55".comb(/ \d+ % '-' /)` is `(1-2-3 4 4-5 5)`.
is-deeply "1-2-3 44-55".comb(/ \d+ % '-' /).List,
    ("1-2-3", "4", "4-5", "5"),
    '.comb with a non-capturing separated quantifier still works';

# The same pattern via ~~ single match and m:g must agree on the match strings.
is ("127.0.0.1" ~~ / (\d ** 1..3) ** 4 % '.' /).Str, "127.0.0.1",
    'single ~~ match of a separated capture quantifier';

my @g;
@g.push(.Str) for "127.0.0.1 5.6.7.8" ~~ m:g/ (\d ** 1..3) ** 4 % '.' /;
is-deeply @g, ["127.0.0.1", "5.6.7.8"], 'm:g match strings agree with .comb';

# .subst using such a pattern should also match.
is "127.0.0.1".subst(/ (\d ** 1..3) ** 4 % '.' /, "IP"), "IP",
    '.subst with a separated capture quantifier matches';
