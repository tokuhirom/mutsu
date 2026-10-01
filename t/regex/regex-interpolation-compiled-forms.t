use Test;

# Interpolated regexes: `<$rx>` and a bare `$rx` splice a Regex value in as a
# match of its own (its captures never reach the caller), `<{ … }>` evaluates
# code to a pattern, and `$x` of an in-regex `:my` lexical matches the lexical's
# value as a literal. The compiled regex engine runs all of these, and both
# engines must agree with raku on what they match, which captures they leave
# and when the code inside them runs. Expected values are raku's.

plan 22;

my $re = /\d+/;
is ~("a12b" ~~ / a <$re> b /), 'a12b', '<$rx> matches the spliced regex';
is ~("a12b" ~~ / a $re b /), 'a12b', 'a bare $rx does too';

my $pair = /(\d)(\d)/;
is ("a12b" ~~ / a <$pair> b /).list.elems, 0, 'the spliced regex\'s captures are discarded';
is ("a12b" ~~ / (a) <$pair> (b) /).list.map(~*).join('|'), 'a|b', 'the caller\'s numbering skips them';

my @log;
my $with-block = /x { @log.push("block@" ~ $/.Str) }/;
ok so "ax" ~~ / a <$with-block> /, 'a spliced regex that holds a code block matches';
is @log.join(','), 'block@x', 'its block ran once, and `$/` in it starts at the splice';

is ~("aaa1" ~~ / [ <$re> | a ]+ /), 'aaa1', '<$rx> as a loop body alternative';
is ~("12ab" ~~ / <$re>+ ab /), '12ab', '<$rx> under a quantifier';
is ~("1234" ~~ / <$re> <$re> /), '1234', 'a second <$rx> after the first took everything backtracks';
is ("1234" ~~ / :r <$re> \d /).defined, False, 'ratchet commits to the first end of <$rx>';

is ~("aab" ~~ / <{ 'a+' }> b /), 'aab', '<{ … }> yields a pattern that is matched here';
my $n = 2;
is ~("aaab" ~~ / <{ 'a' x $n }> a? b /), 'aaab', 'the code sees the surrounding lexicals';

@log = ();
ok so "abc" ~~ / a <{ @log.push("closure@" ~ $/.Str); 'b' }> c /, '<{ … }> with a side effect';
is @log.elems, 1, 'it ran once';

is ~("ab" ~~ / :my $p = 'a'; $p b /), 'ab', '$p of a :my lexical matches its value';
is ~("aab" ~~ / :my $q = 'a'; $q+ b /), 'aab', 'and can be quantified';
is ~("abab" ~~ / :my $r = 'ab'; $r ** 2 /), 'abab', 'and counted';

# A code block that matches a regex which holds code of its own.
@log = ();
ok so "ab" ~~ / a <?{ my $hit = "x" ~~ / x { @log.push("inner") } /; @log.push("outer"); True }> b /,
    'an assertion that runs a regex with a block';
is @log.join(','), 'inner,outer', 'the inner block runs inside the assertion';

# A `:my` lexical is visible inside a nested group of the same regex.
is ~("ab" ~~ / :my $v = 'a'; ( $v ) b /), 'ab', '$v is read inside a capturing group';
is ~("a,a" ~~ / :my $w = 'a'; ( $w )+ % ',' /), 'a,a', 'and inside a separated quantifier\'s atom';
ok so "ab" ~~ / :my $u = 'a'; [ <?{ $u eq 'a' }> a ] b /, 'and by code inside a non-capturing group';
