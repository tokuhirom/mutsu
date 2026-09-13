# Zero-width assertions and backtracking: the two costs that are invisible in
# the matched text.
#
# Look-around is evaluated by running a whole sub-pattern at the current
# position and then throwing the result away, so its cost is per *candidate
# position*, not per match -- and a NEGATIVE one is the expensive case, because
# establishing that nothing matches means exhausting the search. Look-behind is
# worse still: `<?after X>` asks whether X ends exactly here, which is answered
# by running X forward from some earlier start, so a naive implementation pays
# O(pos) per evaluation. That shape is exactly what made the bundled YAMLish
# grammar superlinear in document size (#7576), and nothing in the suite
# measured it.
#
#   1/2. positive and negative look-ahead
#   3/4. positive and negative look-behind
#   5.   a negative look-behind that FAILS at its one candidate and so makes the
#        engine scan the rest of the subject, paying a look-behind per position
#   6/7. word boundaries (<< and >>), the zero-width assertions real patterns
#        use most
#   8.   a greedy class run that must give characters back for the literal tail
#        ('kg' is inside the class, so the first try overshoots)
#   9.   a scan that cannot match: every start position is tried and the
#        quantifier exhausted at each
#   10.  the same shape under :r, where the quantifier may not give back --
#        the contrast is what makes a ratchet regression visible
#   11.  a separated quantifier (`+ %`), whose ratcheted form used to backtrack
#        exponentially

my @lines;
for ^30 -> $i {
    @lines.push("  indent{$i % 4}: alpha beta{$i} gamma_{$i * 3} 12{$i}kg price=4{$i}USD a,b,c,d tail");
}

my $acc = 0;
for ^20 {
    for @lines -> $l {
        $acc++ if $l ~~ / \d+ <?before 'kg' > /;
        $acc++ if $l ~~ / \d+ <!before 'kg' > /;
        $acc++ if $l ~~ / <?after 'price=' > \d+ /;
        $acc++ if $l ~~ / <!after 'price=' > 'USD' /;
        $acc++ if $l ~~ / 'USD' <!after \d > /;
        $acc++ if $l ~~ / << 'gamma_' /;
        $acc++ if $l ~~ / 'beta' \d+ >> /;
        $acc++ if $l ~~ / <[0..9a..z]>+ 'kg' /;
        $acc++ if $l ~~ / \w+ 'QQQ' /;
        $acc++ if $l ~~ / :r \w+ 'QQQ' /;
        $acc++ if $l ~~ / \w+ % ',' /;
    }
}
say "regex-assertion: acc=$acc";
