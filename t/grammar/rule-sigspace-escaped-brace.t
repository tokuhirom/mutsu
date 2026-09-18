use Test;

# `inject_implicit_rule_ws` (the pass that splices `<.ws>` between adjacent
# atoms in a `rule`'s implicit :sigspace body) decides whether to insert a
# marker by looking at the raw characters immediately before/after each
# whitespace run. Its suppression rule for `{`/`}` exists so whitespace
# *inside a code block* (`{ ... }`) stays pure layout. But it read those
# characters straight out of the text it had already emitted, with no memory
# of whether a `{`/`}` there was escaped — so an ESCAPED `\{` (an ordinary
# literal atom, not a code-block opener) was mistaken for one, and the
# `<.ws>` that should follow it silently disappeared. That broke matching any
# `rule` where an escaped metacharacter (`\{`, `\(`, `\[`, `\|`, `\^`, `\<`,
# `\%`) was immediately followed by whitespace and another atom
# (github.com/tokuhirom/mutsu/issues/8700 — found via a WebDriver2::SUT
# grammar whose `branch-type` alternation used `\{ <component-def>+ \}`).

plan 9;

grammar Brace {
    rule r { 'frame' \{ 'x' \} }
}
{
    my $m = Brace.subparse('frame { x }', rule => 'r');
    ok $m, 'escaped \{ ... \} literal braces around a nested atom still match';
}

grammar BraceNoClose {
    rule r { 'frame' \{ 'x' }
}
{
    my $m = BraceNoClose.subparse('frame { x', rule => 'r');
    ok $m, 'an escaped \{ followed by whitespace and another atom matches';
    is $m.to, 9, 'the whitespace after the escaped \{ is consumed by the implicit <.ws>';
}

# The bug is specific to escaped syntax characters being misread as their
# unescaped (structural) meaning; exercise a few of the other characters
# `should_insert` treats specially when unescaped.
grammar Paren {
    rule r { 'a' \( 'b' }
}
{
    my $m = Paren.subparse('a ( b', rule => 'r');
    ok $m, 'escaped \( followed by whitespace matches';
}

grammar Bracket {
    rule r { 'a' \[ 'b' }
}
{
    my $m = Bracket.subparse('a [ b', rule => 'r');
    ok $m, 'escaped \[ followed by whitespace matches';
}

grammar Pipe {
    rule r { 'a' \| 'b' }
}
{
    my $m = Pipe.subparse('a | b', rule => 'r');
    ok $m, 'escaped \| followed by whitespace matches';
}

grammar Percent {
    rule r { 'a' \% 'b' }
}
{
    my $m = Percent.subparse('a % b', rule => 'r');
    ok $m, 'escaped \% followed by whitespace matches';
}

# A real (unescaped) code block must still suppress `<.ws>` injection around
# it — the whitespace touching it stays a literal, REQUIRED space rather than
# becoming the usual optional `<.ws>` — so the fix must not regress the case
# `should_insert`'s `{`/`}` rules exist for.
grammar CodeBlock {
    rule r { 'a' { 1 } 'b' }
}
{
    my $with_space = CodeBlock.subparse('a b', rule => 'r');
    ok $with_space, 'a real code block still matches with its required literal space';
    my $without_space = CodeBlock.subparse('ab', rule => 'r');
    nok $without_space,
        'a real code block still refuses to match without the literal space (no regression)';
}

done-testing;
