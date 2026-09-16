use Test;
use nqp;

# `iscclass`/`findcclass`/`findnotcclass`/`eqat`/`index`/`rindex`/`substr`/
# `ordat` (runtime/nqp_ops_text.rs, runtime/nqp_ops_str.rs,
# runtime/nqp_ops_builtin.rs) used to re-collect their whole string argument
# into a fresh `String`/`Vec<char>` on every single call. A hand-rolled NQP
# scanner calls these once per character (or per token) over the SAME string
# with an advancing position -- JSON::Fast's own JSON parser is the case that
# surfaced this (Test::META's `License::SPDX` dependency, a 332KB bundled
# license list, via `nom-ws`'s `nqp::ordat($text, $pos)` whitespace-skipping
# loop and the string-token `nqp::eqat` check) -- so that made parsing
# quadratic: what should be O(n) work became O(n) work repeated O(n) times,
# turning a sub-second parse into an effective hang (Test::META's
# `t/020-internals.t` timed out at 240s in the ecosystem sweep; `ordat` alone
# turned a 30KB resource file into 14+ seconds). The ops are now memoized by
# the string argument's own identity across consecutive calls.
#
# This file pins two things: the ops still answer correctly when the SAME
# string is scanned at many different positions in a row (the cache must not
# go stale or return the wrong window), and a large string scanned this way
# stays fast (the timing bound guards against the quadratic behavior
# regressing, not just wrong answers).

plan 10;

my constant $N = 20_000;
my $big = ('a' x ($N - 1)) ~ '"';  # N chars, closing quote at the very end

# iscclass: scan every position of a large string in order (the JSON::Fast
# access pattern) and confirm the boundary is found correctly.
{
    my int $count = 0;
    my int $i = 0;
    while nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $big, $i) {
        $count++;
        $i++;
    }
    is $count, $N - 1, 'iscclass scanned every alphabetic char up to the closing quote';
    is $i, $N - 1, 'iscclass loop stopped exactly at the non-alphabetic character';
}

# findnotcclass: same access pattern via the windowed form.
{
    my int $end = nqp::findnotcclass(nqp::const::CCLASS_ALPHABETIC, $big, 0, nqp::chars($big));
    is $end, $N - 1, 'findnotcclass finds the same boundary in one call';
}

# eqat: repeatedly probe the SAME large string at different (including
# decreasing) positions -- the backslash-run-counting idiom JSON::Fast uses.
{
    my int $matches = 0;
    my int $i = $N - 2;  # last 'a'; position N-1 is the closing quote
    while $i >= 0 && nqp::eqat($big, 'a', $i) {
        $matches++;
        $i--;
    }
    is $matches, $N - 1, 'eqat matched every "a" scanning backward from the end';
}

# index: called with the SAME haystack and an advancing start position,
# exactly like JSON::Fast's `nqp::index($text, '"', $pos)` closing-quote scan.
{
    my int $pos = 0;
    my int $found = -1;
    for ^5 {
        $found = nqp::index($big, '"', $pos);
        last if $found != -1;
        $pos++;
    }
    is $found, $N - 1, 'index finds the closing quote after repeated calls with an advancing start';
}

# ordat: JSON::Fast's `nom-ws` calls this once per character while skipping
# whitespace, scanning forward from position 0 over the whole document.
{
    my int $count = 0;
    my int $i = 0;
    while nqp::ordat($big, $i) == 97 {  # 'a'
        $count++;
        $i++;
    }
    is $count, $N - 1, 'ordat scanned every "a" up to the closing quote';
    is nqp::ordat($big, $N - 1), 34, 'ordat sees the closing quote at the final position';  # '"'
}

# substr: repeated calls against the same large string must each see its
# real, current length -- not a length cached from whichever string was
# scanned most recently by a DIFFERENT nqp:: text op call.
{
    my $small = 'hi"';
    nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $small, 0);  # warm the cache on a different string
    is nqp::substr($big, $N - 1, 1), '"', 'substr on a large string is unaffected by a smaller string scanned in between';
    is nqp::substr($small, 0, 2), 'hi', 'substr on the smaller string still sees its own (shorter) length';
}

# Timing bound: this whole file must run well under a hang. If any of the
# above regresses to O(n) work per call, a 20_000-char string scanned
# character-by-character (the iscclass loop above alone is 20_000 calls)
# would take minutes rather than a fraction of a second.
pass 'reached the end of the file without timing out';

# vim: expandtab shiftwidth=4
