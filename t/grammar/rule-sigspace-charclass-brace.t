use Test;

# `inject_implicit_rule_ws` (the pass that splices `<.ws>` between adjacent
# atoms in a `rule`'s implicit :sigspace body) tracks `{`/`}` characters with a
# flat `brace_depth` counter to detect real code blocks (`{ ... }`) and skip
# ws-injection inside them. That counter had no awareness of `<...>`
# regex-syntax regions, so a `{`/`}` appearing INSIDE a `<[...]>` bracketed
# char class (e.g. `<[{]>`, matching a literal `{`) was miscounted as a real
# code-block delimiter. That desynced `brace_depth` for the rest of the
# pattern, silently dropping the `<.ws>` that should follow the char class
# (github.com/tokuhirom/mutsu/issues/8755 — a different root cause from
# #8700's escaped-brace bug: no backslash is involved here at all).

plan 6;

grammar OpenAndClose {
    rule cd { 'frame' <[{]> 'x' <[}]> }
}
{
    my $m = OpenAndClose.subparse('frame { x }', rule => 'cd');
    ok $m, 'a <[{]>/<[}]> bracket char class matching literal braces still allows <.ws>';
}

grammar OpenOnly {
    rule cd { 'frame' <[{]> 'x' }
}
{
    my $m = OpenOnly.subparse('frame { x', rule => 'cd');
    ok $m, 'a <[{]> char class followed by whitespace and another atom matches';
    is $m.to, 9, 'the whitespace after the <[{]> char class is consumed by the implicit <.ws>';
}

# The reduction from the issue: the char class sits inside a subrule called
# from TOP, matching the exact shape that was reported broken.
grammar TestGrammar {
    rule TOP { ^ <cd> $ }
    rule cd {
        'frame' <[{]> 'x' <[}]>
    }
}
ok TestGrammar.parse('frame { x }'), 'the full grammar.parse reduction from the issue matches';

# A real (unescaped) code block must still suppress `<.ws>` injection around
# it — the fix must not regress the case `brace_depth` tracking exists for.
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
