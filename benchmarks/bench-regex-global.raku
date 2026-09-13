# Repeated matching across a whole subject: :g, comb, subst.
#
# These share one mechanism the single-match benchmark never reaches -- resume-
# from-the-last-end scanning over a multi-kilobyte subject, producing one result
# per occurrence -- and differ in what they build from it:
#
#   .match(:g)   a list of Match objects (N materializations)
#   .comb(rx)    a list of Str                (no Match survives)
#   .subst(:g)   a rebuilt Str, replacement computed per occurrence
#   s:g///       the same, in place, through the assignment/container path
#   .subst with a closure replacement, which reads $0 per occurrence and so
#              forces that capture to materialize on every hit
#
# A regression in the scan loop moves all of them; a regression in Match
# construction or in the replacement path moves only its own line.
#
# The subject here is ~2.6 KB and each op runs many times. `split` deliberately
# lives in bench-regex-split-subst.raku instead: it is superlinear in subject
# length today, so at any interesting size it would dominate this file and mask
# everything else in it.

my @lines;
for ^40 -> $i {
    @lines.push("user{$i % 13}=alpha-{$i} score={$i * 17 % 500} note=\"the quick brown fox {$i}\" flags=a,b,c");
}
my $text = @lines.join("\n");

my $n = 0;
for ^45 {
    $n += $text.match(/ \d+ /, :g).elems;
    $n += $text.comb(/ <[a..z]>+ /).elems;
    $n += $text.subst(/ 'score=' \d+ /, 'score=0', :g).chars;
    $n += $text.subst(/ 'user' (\d+) /, { 'u' ~ $0 }, :g).chars;
    my $copy = $text;
    $copy ~~ s:g/ <[0..9]>+ /#/;
    $n += $copy.chars;
}
say "regex-global: n=$n";
