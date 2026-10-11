use Test;

# A quantified one-character atom followed by another one-character atom:
# the engine consumes the run in a tight ASCII loop and skips the give-backs
# that the following atom provably rejects. None of that changes a result.

plan 24;

is ("ab 1" ~~ /\w+ \s \d/).Str, "ab 1", 'greedy run then disjoint atom';
nok ("ab  1" ~~ /\w+ \s \d/).defined, 'disjoint atom fails after the whole run';
is ("a-b c" ~~ /\w+ \s/).Str, "b ", 'scan restarts after a futile start';
is ("aaa" ~~ /a+ a/).Str, "aaa", 'overlapping atoms still give back';
is ("xx1" ~~ /(\w+) \d/)[0].Str, "xx", 'give-back to a digit inside \w+';
is ("foo.bar" ~~ /\w+ \. \w+/).Str, "foo.bar", 'literal after a run';
is ("ab c" ~~ /(\w+) \s (\w)/).list.map(*.Str).join("|"), "ab|c", 'captures survive';
is ("aab" ~~ /a+ b/).Str, "aab", 'run then literal';
is ("aaab" ~~ /a**1..2 ab/).Str, "aaab", 'bounded run gives back for the next atom';
is ("a1b2" ~~ /<[a..z]>+ <[0..9]>/).Str, "a1", 'class run then class';
is ("e\x[301]x y" ~~ /\w+ \s/).Str, "e\x[301]x ", 'combining mark is not split';
is ("\x[e9]\x[e9]1" ~~ /\w+ \d/).Str, "\x[e9]\x[e9]1", 'non-ASCII run';
is ("abc" ~~ /\w+ <[c]>/).Str, "abc", 'next atom accepts the last char of the run';
is ("ab" ~~ /\w+ \s?/).Str, "ab", 'optional next atom is not pruned';


# A failed start's leading `X+` run: later starts inside that run are skipped.
is ("aaa1 bb2 3" ~~ /\w+ \s \d/).Str, "bb2 3", 'scan skips starts inside a failed run';
nok ("abc abd" ~~ /\w+ \s \d/).defined, 'no match anywhere';
is ("ab-cd 5" ~~ /\w+ \s \d/).Str, "cd 5", 'match starts after a failed run';
is ("xxxy" ~~ /x+ y/).from, 0, 'leading run match from';
is ("zzxxy" ~~ /x+ y/).from, 2, 'first start after the failed prefix';
is ("aab aab" ~~ /a+ b$/).from, 4, 'AssertEnd tail';
is ("aaa" ~~ /a+ a**2/).Str, "aaa", 'counted tail still gives back';
is ("e\x[301]e\x[301]- e\x[301] 1" ~~ /\w+ \s \d/).Str, "e\x[301] 1", 'marks inside the run';
is ("ab12 cd34 ef" ~~ /\d+ \s <[a..z]>/).Str, "12 c", 'digits run then space then letter';
is ("1 2 3" ~~ /\d+ \s \d+/).Str, "1 2", 'unbounded tail run';

done-testing;
