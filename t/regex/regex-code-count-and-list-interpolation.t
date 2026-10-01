use Test;

# `$( … )` / `@( … )` interpolate the result of code as a pattern (a list as an
# alternation), and `** { … }` takes its repeat count from code evaluated where
# the quantifier is reached. The compiled regex engine runs both, and both
# engines must agree with raku on what they match, which candidate ends they
# offer to a later atom, and how often the code runs. Expected values are raku's.

plan 25;

my @alts = <ab a>;
is ~("abc" ~~ / @(@alts) c /), 'abc', '@( … ) interpolates a list as an alternation';
is ~("abc" ~~ / $( 'ab' ) c /), 'abc', '$( … ) interpolates a scalar as a literal';
is ~("aab" ~~ / @( <a aa> ) b /), 'aab', 'a later atom backtracks into the next candidate';
is ~("xaab" ~~ / x @(<a aa>) b /), 'xaab', 'also after a literal';
is ~("xab" ~~ / x [ @(<a ab>) ]+ /), 'xab', 'under a quantifier';
is ~("abab" ~~ / :r @(<a ab>) <[ab]>+ /), 'abab', 'ratchet commits to the first candidate';

my @log;
ok so "ab" ~~ / a $( @log.push("interp"); 'b' ) /, 'code with a side effect yields the pattern';
is @log.join(','), 'interp', 'the code ran once';
@log = ();
is ~("aab" ~~ / @( @log.push("pick"); <aa a> ) b /), 'aab', 'the candidates are computed once for the atom';
is @log.join(','), 'pick', 'however many of them the continuation rejects';

my $n = 3;
is ~("aaaa" ~~ / a ** {$n} /), 'aaa', '** { … } reads a lexical';
nok ("aaaa" ~~ / ^ a ** {$n} $ /).defined, 'the count binds the match';
is ~("aaaa" ~~ / a ** {2..3} a /), 'aaaa', 'a range is a minimum and a maximum';
is ~("abab" ~~ / [ab] ** {2} /), 'abab', 'a group can be counted';
is ~("a" ~~ / a ** {0} /), '', 'a count of zero matches nothing';
nok ("aaa" ~~ / :r a ** {1..*} a /).defined, 'ratchet gives nothing back to a later atom';
is ~("aaaa" ~~ / a **? {1..*} /), 'a', 'a frugal ** { … } takes the fewest';
is ~("abcabc" ~~ / [ <alpha> ** {2} ]+ /), 'abcabc', 'inside a loop the count is evaluated per iteration';
my $k = 2;
is ~("ababab" ~~ / [ab] ** {$k} ab /), 'ababab', 'a later atom after the counted group';

@log = ();
ok so "aab" ~~ / a ** { @log.push("count"); 2 } b /, 'count code with a side effect';
is @log.join(','), 'count', 'it ran once, where the quantifier was reached';
@log = ();
ok so "aaab" ~~ / a ** { @log.push("again"); 1..3 } b /, 'a range from code';
is @log.join(','), 'again', 'and the code still ran once';

@log = ();
ok so "xaab" ~~ / x [ a ** { @log.push("it"); 1 } ]+ b /, 'count code inside a loop';
is @log.join(','), 'it,it,it', 'it ran once per iteration, and once more on the failing one';
