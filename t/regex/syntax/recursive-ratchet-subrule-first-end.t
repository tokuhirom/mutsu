use Test;

# A ratcheted caller can only ever use the first end of a `<subrule>` it
# calls, so a recursive token must not enumerate every end of the nested
# calls it contains (#9579). Before the fix both shapes below were
# exponential in the nesting depth, and the embedded block fired once per
# end computed rather than once per end entered.

plan 8;

grammar Braces {
    token TOP { <A> }
    token A { '{' [ <A> | . ]*? '}' }
}

grammar BracesSeq {
    token TOP { <A> }
    token A { '{' [ <A> || . ]*? '}' }
}

{
    my $s = '{' ~ ('{ab{c}d}' x 40) ~ '}';
    my $m = Braces.parse($s);
    ok $m, 'recursive frugal token with `|` parses a long input';
    is $m<A><A>.elems, 40, 'every nested block is its own <A>';
    ok BracesSeq.parse($s), 'the same shape with `||` parses a long input';
}

{
    my $n = 0;
    my grammar Counted {
        token TOP { <A> }
        token A { '{' [ <A> | <-[{}]> ]*? '}' { $n++ } }
    }
    ok Counted.parse('{{a}{b{c}}}'), 'counted grammar parses';
    is $n, 4, 'the block fires once per <A> entered, not once per end computed';
}

# Left recursion is still grown correctly: the new call-graph test must see
# through a nullable atom in front of the recursive call. (mutsu grows
# left-recursive rules with a seed; Rakudo recurses forever on these.)
{
    my grammar LeftRec {
        token TOP { <expr> }
        token expr { <term> | 'x'? <expr> '+' <term> }
        token term { \d }
    }
    my $m = LeftRec.parse('1+2+3');
    ok $m, 'left recursion behind a nullable atom still parses';
    is ~$m<expr><term>, '3', 'and grows to the longest match';
}

{
    my grammar Mutual {
        token TOP { <a> }
        token a { <b> 'x' | 'y' }
        token b { <a> }
    }
    ok Mutual.parse('yxx'), 'indirect left recursion still parses';
}
