use Test;

# A heredoc started in a regex code block takes its body from the lines after
# the line the block closes on; those lines are not regex atoms.

plan 6;

grammar G {
    token TOP { { $*out = Q:to<E> }
        hi
        E
    x }
}
my $*out;
ok G.parse("x"), 'grammar token with a heredoc in a code block parses';
is $*out, "hi\n", 'the heredoc body is the code block\'s';

grammar H {
    token TOP { [ { $*out ~= Q:to<E> ~ "!" }
        there
        E
    ]**1..2 x }
}
$*out = '';
ok H.parse("x"), 'heredoc in a quantified group';
is $*out, "there\n!there\n!", 'the block ran twice with its heredoc';

my $t = token { { $*out = q:to/END/ }
    anon
    END
 y };
ok "y" ~~ $t, 'anonymous token with a heredoc in a code block';

use MONKEY-SEE-NO-EVAL;
my $r = EVAL Q:to/SRC/;
token { [[ {X::NYI.new(:feature(｢abc｣.fmt(Q:to<ERR>))).throw}
    req %s
    ERR
 ]**1..* ] }
SRC
isa-ok $r, Regex, 'EVAL of a token whose code block starts a heredoc';
