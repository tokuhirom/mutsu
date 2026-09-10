use Test;

# The colonless "compact" match adverb parser (`parse_compact_match_adverbs`)
# used to greedily consume ANY of the letters s/i/g/m/p/c immediately after a
# bareword/identifier starting with `m`, then treat the next character as a
# regex delimiter and scan the REST OF THE FILE for a matching close
# delimiter. Real raku recognizes exactly one colonless compact form —
# `ms/pattern/` (shorthand for `m:s/pattern/`, sigspace) — and rejects every
# other letter combination as a syntax error (`mi/x/`, `mg/x/`, `mp5/x/`,
# `mss/x/` all die "Missing required term after infix" in raku).
#
# This meant an ordinary bareword/sigilless-param method-call chain like
# `msg.gist // msg.gist` (m+s+g happens to satisfy the old, too-permissive
# consumer, with `.` as the "delimiter" and the SECOND `msg.gist`'s `.`
# providing a spurious matching close) silently mis-parsed as a regex
# literal instead of two ordinary method calls joined by `//` (defined-or).
# See news/2026-08/compact-match-adverb-overreach-mis-parses-bareword.md.

plan 6;

{
    my \msg = "hello";
    my $x = msg.gist // msg.gist;
    is $x, "hello", 'bareword \msg: msg.gist // msg.gist parses as two method calls, not a regex';
}

{
    sub f(\msg) {
        my $x = msg.gist // msg.gist;
        return $x;
    }
    is f("hi"), "hi", 'sigilless param \msg: same shape inside a routine body';
}

{
    # A `whenever`-bound sigilless param is the shape that broke the vendored
    # Cro::HTTP::Router.rakumod parse entirely (Cro.rakumod's
    # PipelineTraceTransform uses `whenever $in -> \msg { ... msg.perl }`).
    my $s = supply { emit "x" };
    my $result;
    react {
        whenever $s -> \msg {
            $result = msg.gist // msg.gist;
        }
    }
    is $result, "x", 'whenever -> \msg { msg.gist // msg.gist }: not mis-parsed as a regex';
}

{
    # `ms/pattern/` (the one real colonless compact adverb) must still work.
    ok 'abc  def' ~~ ms/abc def/, 'ms// (real colonless compact sigspace adverb) still works';
}

{
    # Other bareword-adjacent letter combos must NOT be swallowed as compact
    # adverbs either — `mice.gist` (m+i+c+e — 'e' stops the old consumer
    # before a delimiter, so this specific one never broke, but pin it
    # alongside the fixed case for regression coverage).
    my \mice = "squeak";
    is mice.gist, "squeak", 'bareword \mice: not affected by compact-adverb letters';
}

{
    my \mask = "on";
    my $y = mask.gist // mask.gist;
    is $y, "on", 'bareword \mask: m+a is not a compact-adverb letter, unaffected either way';
}

done-testing;
