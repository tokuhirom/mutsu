# Building a result from EVERY occurrence: split and global substitution.
#
# `.split(rx)` and `.subst(rx, :g)` walk the subject once like `.comb` does, but
# unlike comb they assemble a result out of the gaps between the matches. Both
# were quadratic in subject length until #8247: each match re-derived the whole
# subject's char vector, so an 80 KB `split(/\s+/)` took 11.9 s against rakudo's
# 0.26 s (46x) and a 640 KB global substitution 23.2 s against 0.79 s (29x),
# while `.comb` over the same subject stayed flat. One shared `MatchTarget` per
# call fixed it: those two are now 0.05 s and 0.13 s.
#
# This file is the guard that they stay linear. It is sized to make a
# reintroduction obvious rather than marginal: 256 KB through `split`, 512 KB
# through `subst(:g)`. At the old per-match cost it would not finish inside the
# bench timeout at all; linear, it is ~0.2 s. Read it alongside
# `bench-regex-long-subject`, which measures the scan rather than the assembly.

my $unit = "alpha beta gamma delta epsilon zeta eta theta iota 12345\n";

# 256 KB, ~46000 separators: the split walk and its gap assembly.
my $split_subject = $unit x (262144 div $unit.chars);
my $fields = $split_subject.split(/ \s+ /).elems;

# 512 KB, one occurrence per line: the substitution walk, with ~12x fewer
# occurrences per character, so it is the replacement assembly being measured
# rather than the separator count.
my $subst_subject = $unit x (524288 div $unit.chars);
my $chars = $subst_subject.subst(/ \d+ /, '#', :g).chars;

# The same 256 KB text split on a single-char literal regex, which has no
# gap-assembly shortcut to fall back on either.
my $lines = $split_subject.split(/ \n /).elems;

say "regex-split-subst: fields=$fields chars=$chars lines=$lines";
