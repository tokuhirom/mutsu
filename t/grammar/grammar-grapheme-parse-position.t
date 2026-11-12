use Test;

plan 9;

grammar G { token TOP { 'a' } }

my $sub = G.subparse("👩‍💻ab", :pos(1));
is $sub.Str, 'a', ':pos(1) starts after the first grapheme';
is $sub.from, 1, 'the subparse reports a grapheme offset';
is $sub.to, 2, 'the subparse end is also a grapheme offset';

my $full = G.parse("👩‍💻a", :pos(1));
is $full.Str, 'a', 'parse accepts a grapheme start offset';
is $full.from, 1, 'parse reports the grapheme start offset';

is G.subparse("\r\na", :pos(1)).Str, 'a', 'CRLF counts as one grapheme';

grammar Joiner { token TOP { "\x[200D]" } }
nok Joiner.subparse("👩‍💻a", :c(1)),
    ':c(1) starts after the first grapheme, not inside its code points';

grammar MethodStart {
    method TOP { self.word }
    token word { 'a' }
}
is MethodStart.subparse("👩‍💻a", :pos(1)).Str, 'a',
    'a cursor method passes its internal code-point position once';

MethodStart.^find_method('word').wrap(-> |c { callsame });
is MethodStart.subparse("👩‍💻a", :pos(1)).Str, 'a',
    'a wrapped cursor method preserves the internal code-point position';
