use v6;
use Test;
use nqp;

# `Match.^lookup("CURSOR_MORE")` -- the second half of the NQP cursor protocol
# (#7931): advance a cursor that just matched to the next match of the SAME
# regex, without re-scanning the prefix. `String::Utils`'s `replace-all` is
# written entirely in this idiom, and it is lizmat's house style for "walk
# every match", so the shape here is what ecosystem code depends on.
#
# The companion file `match-cursor-protocol.t` covers `!cursor_init` and
# calling a regex on a cursor. Every expectation below was measured against
# rakudo 2026.07.

plan 24;

my $cursor-init = Match.^lookup("!cursor_init");
my $more        = Match.^lookup("CURSOR_MORE");

is Match.^can("CURSOR_MORE").elems, 1, 'Match.^can finds CURSOR_MORE';

# ---- walking every match ------------------------------------------------
# The full trace of a walk, `[$!from,$!pos]` per step, ending in the failed
# cursor that terminates the loop.
sub walk($needle, Str $haystack, Int $steps = 8) {
    my $cursor := $needle($cursor-init(Match, $haystack, :0c));
    my @trace;
    for ^$steps {
        @trace.push(
            "[" ~ nqp::getattr_i($cursor, Match, '$!from')
                ~ "," ~ nqp::getattr_i($cursor, Match, '$!pos') ~ "]"
        );
        last if nqp::getattr_i($cursor, Match, '$!pos') < 0;
        $cursor := $more($cursor);
    }
    @trace.join(" ")
}

is walk(/o/, "foo boo"), "[1,2] [2,3] [5,6] [6,7] [7,-3]",
    'a walk visits every match and ends in a failed cursor';
is walk(/\d+/, "a12b345"), "[1,3] [4,7] [7,-3]",
    'a multi-char match resumes at its own end, not one past it';
is walk(/o/, "ooo"), "[0,1] [1,2] [2,3] [3,-3]",
    'adjacent matches are all found';
is walk(/z/, "abc"), "[0,-3]",
    'a haystack with no match yields one failed cursor';

# A ZERO-WIDTH match must not be found again in the same place: rakudo bumps
# the resumption position by one when $!from == $!pos. Without that, this walk
# never terminates.
is walk(/x*/, "axb"), "[0,0] [1,2] [2,2] [3,3] [4,-3]",
    'a zero-width match is bumped past, so the walk terminates';
is walk(/\d*/, "ab"), "[0,0] [1,1] [2,2] [3,-3]",
    'a regex that only ever matches zero-width walks one position at a time and stops';

# ---- the resumed cursor is a real cursor --------------------------------
my $first := /(\w)(\d)/($cursor-init(Match, "a1 b2", :0c));
my $second := $more($first);
is $second.from, 3, 'the resumed cursor reports the next match .from';
is $second.to, 5, 'and its .to';
is $second.Str, 'b2', 'and stringifies to the next match';
ok ?$second, 'and is True';
is $second.^name, 'Match', 'and is a Match';

# The resumed call goes through the same path as the original, so a regex's
# adverbs are honoured on every step, not just the first.
is walk(rx:i/o/, "fOo"), "[1,2] [2,3] [3,-3]",
    ':i survives the resume';

# The cursor CURSOR_MORE is asked about is not disturbed by the resume.
is $first.from, 0, 'the original cursor still reports its own .from';
is $first.Str, 'a1', 'and its own .Str';

# Positions are character positions, not bytes, on the resumed step too.
is walk(/b/, "\c[SNOWMAN]b\c[SNOWMAN]b"), "[1,2] [3,4] [4,-3]",
    'resumed positions count characters, not bytes';

# ---- what cannot be resumed ---------------------------------------------
# rakudo dies on both of these too (a cursor that never ran, and a failed one,
# carry a null `$!regexsub`), so refusing is the faithful answer -- but with a
# message that says which case it is.
#
# NOT pinned here: rakudo can also resume an ordinary `"abc".match(/b/)`,
# because every rakudo Match is a spent Cursor and carries its `$!regexsub`.
# mutsu only remembers the regex for a Match produced BY a cursor call; see
# `regex_cursor`'s module doc for why.
my $virgin := $cursor-init(Match, "abc", :0c);
dies-ok { $more($virgin) }, 'a cursor that never ran cannot be advanced';
my $failed := /z/($cursor-init(Match, "abc", :0c));
dies-ok { $more($failed) }, 'a failed cursor cannot be advanced';

# ---- the idiom this exists for ------------------------------------------
# `String::Utils::replace-all`, verbatim (lib/String/Utils.rakumod:575).
my sub replace-all(str $haystack, Regex:D $needle, str $replacement) {
    my $cursor := $needle($cursor-init(Match, $haystack, :0c));
    my int $pos = nqp::getattr_i($cursor, Match, '$!pos');
    if $pos >= 0 {
        my int $start;
        my str @parts;
        nqp::while(
          $pos >= 0,
          nqp::stmts(
            nqp::push_s(@parts,
              nqp::substr(
                $haystack,
                $start,
                nqp::getattr_i($cursor, Match, '$!from') - $start
              )
            ),
            nqp::push_s(@parts, $replacement),
            $start = $pos,
            ($cursor := $more($cursor)),
            ($pos = nqp::getattr_i($cursor, Match, '$!pos'))
          )
        );
        nqp::push_s(@parts, nqp::substr($haystack, $start));
        nqp::join("", @parts)
    }
    else {
        $haystack
    }
}

is replace-all("foobarfoo", /foo/, "X"), "XbarX", 'replace-all replaces every match';
is replace-all("a1b22c333", /\d+/, "-"), "a-b-c-", 'replace-all handles varying match widths';
is replace-all("hello", /z/, "X"), "hello", 'replace-all leaves a non-matching string alone';
is replace-all("hello world", /o/, "0"), "hell0 w0rld", 'replace-all spans the whole string';
is replace-all("aaa", /a*/, "<>"), "<><>", 'replace-all terminates on a zero-width-capable regex';
is replace-all("", /x/, "Y"), "", 'replace-all of an empty string is empty';

done-testing;
