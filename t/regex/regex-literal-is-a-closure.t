use v6;
use Test;

plan 9;

# A Raku regex is a closure over the scope it was written in: the code embedded
# in its pattern (`{ … }`, `<?{ … }>`, `:my` initializers) resolves its free
# variables against the DEFINING scope, not against whatever happens to be
# lexically visible where the match runs.

# A regex built in a sub and matched by its caller still sees the sub's array.
{
    sub make-rx() {
        my @h = "aa", "bb";
        / ^ 'x' <?{ @h[1] eq 'bb' }> $ /
    }
    my $rx = make-rx();
    ok "x" ~~ $rx, 'an assertion reads the array of the scope that built the regex';
}

# The defining scope wins over a same-named lexical live at the match site.
{
    my $y = 1;
    my $rx = / a { $*SEEN = $y } /;
    sub match-elsewhere($r) {
        my $y = 2;
        "a" ~~ $r;
    }
    my $*SEEN;
    match-elsewhere($rx);
    is $*SEEN, 1, 'the defining scope wins over the match-site lexical';
}

# A scalar captured by an escaping regex.
{
    my $rx;
    {
        my $greeting = "hi";
        $rx = / w { make $greeting } /;
    }
    "w" ~~ $rx;
    is $/.ast, "hi", 'a block-lexical scalar survives the block that declared it';
}

# A write from an embedded block is still a real side effect (closure write),
# not swallowed by the captured-scope bookkeeping.
{
    my $n = 0;
    ok 'abc' ~~ / a { $n = 42 } bc /, 'a code block still matches';
    is $n, 42, 'a code block write is still visible after the match';
}

# Nothing to capture: an ordinary code-bearing regex is unchanged.
{
    my @log;
    ok 'ab' ~~ / a { @log.push('hit') } b /, 'a plain code block still runs';
    is-deeply @log, ['hit'], 'and its side effect lands';
}

# `.match` gets the captured scope too, not just `~~`.
{
    sub build() {
        my $want = 3;
        / (\d) <?{ +$0 == $want }> /
    }
    my $rx = build();
    ok "3".match($rx), '.match installs the captured scope';
    nok "4".match($rx), 'and the assertion still rejects a non-match';
}
