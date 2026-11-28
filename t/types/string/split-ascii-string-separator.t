use Test;

# Splitting an ASCII string (with no "\r") by a string separator skips the
# grapheme index: every byte offset is a grapheme index there. The pieces and
# every adverb's report must be what the general path gives.

plan 12;

is 'the quick brown fox'.split(' ').join('|'), 'the|quick|brown|fox', 'one-byte separator';
is 'a, b, c'.split(', ').join('|'), 'a|b|c', 'multi-byte separator';
is ' a  b '.split(' ').raku, ('', 'a', '', 'b', '').Seq.raku, 'leading, doubled and trailing separators';
is 'abc'.split('x').raku, ('abc',).Seq.raku, 'no hit';
is 'a b c d'.split(' ', 2).join('|'), 'a|b c d', 'a limit';
is 'a b c'.split(' ', :skip-empty).join('|'), 'a|b|c', ':skip-empty';
is 'a-b-c'.split('-', :k).raku, ('a', 0, 'b', 0, 'c').Seq.raku, ':k reports the separator index';
is 'a-b-c'.split('-', :v).raku, ('a', '-', 'b', '-', 'c').Seq.raku, ':v reports the separator';
is 'ab--cd'.split('--', :p).raku, ('ab', 0 => '--', 'cd').Seq.raku, ':p with a two-byte separator';

# Not ASCII, or a "\r\n" cluster: the general path.
is "a\r\nb\r\nc".split("\n").elems, 1, 'a separator inside a \r\n grapheme does not split';
is 'é-ü-x'.split('-').join('|'), 'é|ü|x', 'non-ASCII text';

# The pieces share nothing observable with the invocant.
my $s = 'x y';
my @p = $s.split(' ');
$s = 'changed';
is @p.join('|'), 'x|y', 'pieces outlive a change of the invocant variable';
