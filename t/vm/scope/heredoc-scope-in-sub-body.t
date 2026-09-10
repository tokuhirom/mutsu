use Test;
plan 8;

# A `my` local declared earlier in a sub, referenced later in that same sub
# from a qq:to/ heredoc, is ordinary lexical scoping: the local is visible
# regardless of the heredoc. This was a false-positive compile error
# ("Variable '$x' is not declared...") whenever the heredoc's own marker
# line (before its terminator) had nothing else on it — the overwhelmingly
# common style. Found while surveying CSV::Table / Text::Utils on the
# ecosystem (docs/batteries/csv.md).
sub plain-heredoc-after-my() {
    my $x = "hi";
    return qq:to/HERE/.trim;
    value: $x
    HERE
}
is plain-heredoc-after-my(), "value: hi",
    'a heredoc statement (marker line ends in nothing/`;`) sees an earlier my in the same sub';

sub hyphenated-name-heredoc(:$opt) {
    my $opt-used = $opt.defined ?? $opt !! "(none)";
    print qq:to/HERE/;
    Params: using option '{$opt-used}'
    HERE
}
lives-ok { hyphenated-name-heredoc(:opt<x>) },
    'a hyphenated my local is visible to a later heredoc in the same sub too';

sub print-then-heredoc() {
    my $x = "hi";
    print qq:to/HERE/;
value: $x
HERE
}
lives-ok { print-then-heredoc() },
    'unindented heredoc terminator, closing brace on its own later line, still works';

{
    my $ok = True;
    if $ok {
        my $y = "inner";
        my $z = qq:to/HERE/.trim;
        value: $y
        HERE
        is $z, "value: inner",
            'an if-branch heredoc statement sees an earlier my in the same branch';
    }
}

# The genuine Raku gotcha this check exists for (roast/S02-literals/heredocs.t
# "heredoc fails in block 2a"/"heredoc fails in block 4"): when the heredoc's
# own marker line ALSO closes the enclosing block (a bare `}` right after the
# marker, before the heredoc's own body/terminator), that block's `my` locals
# really are out of scope by the time Raku resolves the heredoc body — this
# must still be a compile-time error.
dies-ok { EVAL q{
    sub f() { my $a = 'foo'; qq:to/END/ }
       $a
       END
} }, 'a my local whose declaring sub closes on the heredoc marker line is still an error';

dies-ok { EVAL q{
    my $x;
    if $x { my $var = 42; say qq:to/END/ }
       $var
       END
} }, 'same for an if-block that closes on the heredoc marker line';

# The same shape works once the variable is declared OUTSIDE the block that
# closes on the marker line (roast "heredoc made to work in block 2b").
lives-ok { EVAL q{
    my $a;
    sub f() { $a = 'foo'; qq:to/END/ }
       $a
       END
    f();
} }, 'a my local declared outside the closing block is still visible';

lives-ok { EVAL q{
    my $x;
    if $x { say q:to/END/ }
       no variable referenced
       END
} }, 'a heredoc with no variable reference is unaffected either way';
