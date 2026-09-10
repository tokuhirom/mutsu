use v6;
use Test;

# `m:pos(N)/.../` anchors the match to start exactly at character offset N;
# `m:continue(N)/.../` (`:c(N)`) starts searching from offset N. Regression:
# mutsu parsed `:pos`/`:continue` as bare boolean flags and discarded the `(N)`
# argument, so both fell back to the previous match's `$/.to` (0), matching
# from the start. (Language/regexes.rakudoc)

plan 7;

given 'abcdef' {
    my $match = m:pos(2)/.*/;
    is $match.from, 2, ':pos(2) anchors .from at offset 2';
    is ~$match, 'cdef', ':pos(2) matches from offset 2';
}

# :pos(N) is an anchor: the pattern must match *exactly* at N or fail.
nok (so 'abcdef' ~~ m:pos(2)/abc/), ':pos(2) fails when the pattern is not at offset 2';
ok  (so 'abcdef' ~~ m:pos(0)/abc/), ':pos(0) matches an anchored pattern at the start';

# :continue(N) may match anywhere at or after N (not anchored).
given 'abcdef' {
    my $m = m:continue(3)/e./;
    is ~$m, 'ef', ':continue(3) searches forward from offset 3';
    is $m.from, 4, ':continue(3) found the match at offset 4';
}

# :continue(N) resumes scanning at N and can match later in the string.
given 'abcdef' {
    is ~(m:continue(2)/\w+/), 'cdef', ':continue(2) resumes scanning at offset 2';
}
