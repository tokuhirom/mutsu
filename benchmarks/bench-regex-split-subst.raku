# Building a result from EVERY occurrence: split and global substitution.
#
# `.split(rx)` and `.subst(rx, :g)` walk the subject once like `.comb` does, but
# unlike comb they assemble a result out of the gaps between the matches. In
# mutsu that assembly is currently superlinear in subject length, which no
# benchmark measured -- so this file exists to measure it, and to make the fix
# visible when it lands. Measured locally (release, 2026-09-13), subject of
# repeated 46-char units, mutsu vs rakudo 2026.07, one call each:
#
#   .split(/\s+/)     5 KB    52 ms vs 590 ms    10 KB   153 ms vs 229 ms
#                    20 KB   545 ms vs 241 ms    40 KB  2074 ms vs 590 ms
#                    80 KB 11877 ms vs 257 ms   (46x)
#   .subst(/\d+/,:g) 20 KB    29 ms              40 KB    77 ms
#                    80 KB   260 ms             640 KB 23174 ms  (29x)
#
# Doubling the subject roughly quadruples the time in both: each occurrence pays
# a cost proportional to the whole subject, not to its own span. `.comb` over the
# same subject is flat (18 ms at 80 KB), so the scan is not the problem -- the
# result assembly is. Filed as its own issue; this benchmark is the regression
# guard either way, and the sizes below are picked so that the file costs a few
# hundred milliseconds at TODAY's cost (a fix should drop it by ~10x, which the
# deterministic instruction-count series will show unambiguously).

my $unit = "alpha beta gamma delta epsilon zeta eta theta iota 12345\n";

# 12 KB: ~2100 fields out of split, the superlinear term already dominant.
my $split_subject = $unit x (12288 div $unit.chars);
my $fields = $split_subject.split(/ \s+ /).elems;

# 64 KB: one occurrence per line, so the same quadratic term with ~12x fewer
# occurrences -- keeps the substitution side measurable without dominating.
my $subst_subject = $unit x (65536 div $unit.chars);
my $chars = $subst_subject.subst(/ \d+ /, '#', :g).chars;

# The same 12 KB text split on a single-char literal regex, which has no
# gap-assembly shortcut to fall back on either.
my $lines = $split_subject.split(/ \n /).elems;

say "regex-split-subst: fields=$fields chars=$chars lines=$lines";
