use Test;

# `\r\n` written as two escapes in a regex must match a literal CRLF pair,
# the same as writing it quoted (`/"\r\n"/`) or as a plain string smartmatch.
#
# `\r` compiles to a `Literal('\r')` atom, but `\n` compiles to
# `RegexAtom::Newline` (it matches any Unicode logical newline, not only a
# literal LF) rather than `Literal('\n')`. The grapheme-merging pass that
# folds adjacent literal escapes spanning one grapheme cluster into a single
# atom (`\r` immediately before `\n` is one CRLF cluster, so a `Literal('\r')`
# alone may never match half of it) only recognized runs of plain `Literal`
# tokens, so it never saw the `\n` escape's `Newline` atom and left the `\r`
# unpaired -- which then failed its own atomicity check and refused to match
# at all. Found via LWP::Simple's `parse_response`, which splits headers on
# `.split(/\r\n/)`: the split silently produced a single unsplit line, so the
# header hash stayed empty and the response body was never decoded.

plan 8;

ok "A\r\nB" ~~ /\r\n/, 'two escapes matches a literal CRLF';
ok "A\r\nB" ~~ /A\r\nB/, 'a CRLF pair mid-pattern, with literals on both sides';
ok "A\r\r\nB" ~~ /\r\r\n/, 'a leading extra \r before the CRLF pair still matches';
nok "A\r\r\r" ~~ /\r\n/, 'no LF at all still does not match';

my @parts = "HTTP\r\nBBB\r\nCCC".split(/\r\n/);
is-deeply @parts, ["HTTP", "BBB", "CCC"],
    'split on /\r\n/ finds every CRLF-joined line';

# The reverse order (`\n` before `\r`) is not a cluster and needs no merging;
# a regression guard that the CRLF-specific fold does not fire here.
ok "A\n\rB" ~~ /\n\r/, '\n\r (not a cluster) still matches in that order';

# `\r` and `\n` alone still keep their own semantics, unaffected by pairing
# logic that only fires when they are adjacent in that order.
nok "A\rB" ~~ /\r\n/, 'a lone \r with no following \n does not match /\r\n/';
ok "A\nB" ~~ /\n/, 'a lone \n still matches on its own';

done-testing;
