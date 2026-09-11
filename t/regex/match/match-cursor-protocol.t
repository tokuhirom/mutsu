use v6;
use Test;
use nqp;

# The NQP cursor protocol exposed to user code (#7883): `Match.^lookup
# ("!cursor_init")` builds a cursor, invoking a Regex (or a grammar token) on
# that cursor matches at/from it, and `nqp::getattr_i($cursor, Match, '$!pos')`
# reads where it got to. Ecosystem code drives regexes by hand with it --
# `String::Utils`'s `replace`/`replace-all` are written entirely in this idiom.
#
# Every expectation below was measured against rakudo 2026.07.

plan 32;

# `.^lookup`'s result is an NQPRoutine in rakudo, which answers neither
# `.defined` nor `.Bool` -- so `.^can` is the portable presence check, and the
# lookup itself is proved to work by being CALLED below.
my $cursor-init = Match.^lookup("!cursor_init");
is Match.^can("!cursor_init").elems, 1, 'Match.^can finds !cursor_init';

# A leading `!` is an ordinary NQP identifier character, not Raku's
# private-method marker: the method is found by its full name, and nothing
# named `cursor_init` exists.
is Match.^can("cursor_init").elems, 0, 'the bare name (no "!") is not a method';
is Match.^lookup("cursor_init").^name, 'Mu', 'and .^lookup of it answers Mu';

# ---- the cursor a fresh `:c` init produces -------------------------------
# `:c(n)` means "not started, continue from n": $!from is -1, $!pos is n.
my $init := $cursor-init(Match, "xfoox", :0c);
is nqp::getattr_i($init, Match, '$!from'), -1, ':c cursor has $!from == -1';
is nqp::getattr_i($init, Match, '$!pos'), 0, ':c cursor has $!pos == :c value';
is $init.pos, 0, '.pos reads $!pos';

my $init3 := $cursor-init(Match, "xfoox", :c(3));
is nqp::getattr_i($init3, Match, '$!pos'), 3, ':c(3) starts at 3';

# `:p(n)` means "anchored at n": $!from is n, so calling a regex on it does
# NOT scan forward.
my $anchored := $cursor-init(Match, "xfoox", :p(2));
is nqp::getattr_i($anchored, Match, '$!from'), 2, ':p cursor carries its anchor in $!from';
is nqp::getattr_i($anchored, Match, '$!pos'), 2, ':p cursor $!pos is the anchor too';

# ---- invoking a Regex on a cursor ---------------------------------------
my $hit := /foo/($cursor-init(Match, "xfoox", :0c));
is nqp::getattr_i($hit, Match, '$!from'), 1, 'a :c cursor SCANS: $!from is where the match started';
is nqp::getattr_i($hit, Match, '$!pos'), 4, '$!pos is where the match ended';
is $hit.from, 1, '.from agrees';
is $hit.to, 4, '.to agrees';
is $hit.Str, 'foo', '.Str is the matched text';
ok ?$hit, 'a matched cursor is True';

# A cursor whose regex did not match is a *failed* Match: defined, but false,
# with $!pos set to the failure marker and $!from left at the start position.
my $miss := /zzz/($cursor-init(Match, "xfoox", :c(2)));
ok $miss.defined, 'a failed cursor is still defined';
nok ?$miss, 'a failed cursor is False';
is nqp::getattr_i($miss, Match, '$!pos'), -3, 'a failed cursor reports $!pos == -3';
is nqp::getattr_i($miss, Match, '$!from'), 2, 'a failed cursor leaves $!from at the start position';
is $miss.Str, '', 'a failed cursor stringifies empty';

# Scanning starts at the cursor position, not at 0.
is (/foo/($cursor-init(Match, "xfoox", :c(3)))).pos, -3,
    'scanning from past the match fails';
is (/o/($cursor-init(Match, "xfoox", :c(3)))).from, 3,
    'scanning from 3 finds the second "o"';

# `:p` anchors instead of scanning.
is (/foo/($cursor-init(Match, "xfoox", :p(0)))).pos, -3,
    ':p(0) anchors, so /foo/ does not match "xfoox" there';
is (/foo/($cursor-init(Match, "xfoox", :p(1)))).pos, 4,
    ':p(1) anchors exactly where the match is';

# An `rx//` held in a variable works the same as a literal.
my $needle = rx/foo/;
is ($needle($cursor-init(Match, "xfoox", :0c))).pos, 4, 'a Regex in a variable is callable on a cursor';

# Positions are in characters, not bytes.
is (/b/($cursor-init(Match, "\c[SNOWMAN]bc", :0c))).pos, 2,
    'cursor positions count characters, not bytes';

# ---- a grammar token on a cursor ----------------------------------------
grammar CursorG { token foo { foo } }
my $tok = CursorG.^lookup("foo");
my $tok-hit := $tok($cursor-init(CursorG, "xfoox", :0c));
is nqp::getattr_i($tok-hit, Match, '$!from'), 1, 'a token scans from a :c cursor too';
is nqp::getattr_i($tok-hit, Match, '$!pos'), 4, 'and reports where it ended';
is nqp::getattr_i($tok($cursor-init(CursorG, "xzzzx", :0c)), Match, '$!pos'), -3,
    'a token that never matches yields a failed cursor';

# ---- the idiom this exists for ------------------------------------------
# `String::Utils::replace`, verbatim (lib/String/Utils.rakumod:563).
my sub replace(str $haystack, Regex:D $needle, str $replacement) {
    my $cursor := $needle($cursor-init(Match, $haystack, :0c));
    my int $pos = nqp::getattr_i($cursor, Match, '$!pos');
    $pos >= 0
      ?? nqp::substr($haystack, 0, nqp::getattr_i($cursor, Match, '$!from'))
           ~ $replacement
           ~ nqp::substr($haystack, $pos)
      !! $haystack
}

is replace("foobarfoo", /bar/, "BAZ"), "fooBAZfoo", 'String::Utils-style replace hits';
is replace("foobarfoo", /zzz/, "BAZ"), "foobarfoo", 'and leaves the string alone on a miss';
is replace("hello world", /\s+/, "-"), "hello-world", 'and replaces only the first match';

done-testing;
