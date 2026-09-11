use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a heredoc
# whose introducing line carries code AFTER the marker resumes the parse on a
# freshly BUILT buffer — the rest of that line spliced onto the text after the
# terminator. `rest` is then not a tail slice of the string the caller passed, so
# the `&input[..input.len() - rest.len()]` subtractions several parsers used to
# recover their consumed span landed at an arbitrary offset. With multi-byte text
# in the body that offset fell inside a character and the parser PANICKED
# (Collection's `RefreshPlugins.rakumod`, via a `MapFail.new(:note(qq:to/WARN/))
# .throw unless ...`); with ASCII it silently read the wrong span. The sites now
# recover the span by pointer provenance (`consumed_span`) instead.

plan 8;

# A statement-modifier tail after the marker, with a multi-byte body — the exact
# shape that panicked.
class MapFail is Exception {
    has $.note;
    method message { $.note }
}
my $plug = 'p';
my $ok = False;
my $thrown = '';
try {
    MapFail.new(:note(qq:to/WARN/)).throw unless $ok;
        Major part error? No released plugin ｢{ $plug }_v1｣ corresponding to ｢$plug｣ in ｢mode｣
        WARN
    CATCH { default { $thrown = .message } }
}
ok $thrown.contains('｢p_v1｣'), 'a heredoc under a statement modifier keeps its multi-byte body';
ok $thrown.contains('corresponding to ｢p｣ in ｢mode｣'), 'and the whole body survives';

# The same shape with the modifier NOT firing: nothing is thrown.
my $fired = True;
my $second = 'untouched';
MapFail.new(:note(qq:to/SKIP/)).throw unless $fired;
    ｢never｣ thrown ｢at all｣ because the modifier is false
    SKIP
is $second, 'untouched', 'the statement modifier still suppresses the throw';

# A heredoc as the left-hand side of `=>`: the fat-arrow paths recovered the same
# span to decide whether to autoquote a bareword key.
my %h = qq:to/E/ => 'v';
    ｢キー｣ の値、これは十分に長い行なので境界がずれると文字の中に落ちる
    E
is %h.values, ('v',), 'a heredoc `=>` key keeps its value';
ok %h.keys[0].contains('｢キー｣'), 'and the key keeps its multi-byte body';

# A heredoc inside a `for` body whose marker line carries a postfix chain: the
# loop's `&?BLOCK` scan recovered the same span.
my @out;
for 1, 2 {
    @out.push: qq:to/B/.trim;
        行 $_ ｢だけ｣
        B
}
is @out, ['行 1 ｢だけ｣', '行 2 ｢だけ｣'], 'a heredoc in a for body interpolates the topic per iteration';

# Bareword-key autoquoting, the thing those fat-arrow spans actually decide, is
# unchanged for an ordinary key.
my %plain = key => 1;
is %plain<key>, 1, 'a bareword `=>` key is still autoquoted';
is (Bool::True => 2).key, True, 'and a qualified name on the left is still evaluated';
