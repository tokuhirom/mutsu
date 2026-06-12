use Test;

plan 6;

# Fake-infix adverb args (space-separated colonpairs) on a `.=` method call.
# Previously these parsed correctly at statement level but were mis-parsed
# inside parentheses (the parenthesized-assignment path stripped the leading
# colon and parsed the colonpairs as a function call).

# Statement level (already worked).
{
    my Pair $p;
    $p .= new :key<foo> :value<bar>;
    is-deeply $p, :foo<bar>.Pair, 'statement-level fake-infix adverbs on .=';
}

# Inside parentheses (the bug).
{
    my Pair $p;
    is-deeply ($p .= new :key<foo> :value<bar>), :foo<bar>.Pair,
        'parenthesized fake-infix adverbs on .=';
}

# Parenthesized with paren-style colonpair values.
{
    my Pair $p;
    is-deeply ($p .= new :key(1) :value(2)), (1 => 2),
        'parenthesized fake-infix adverbs with () values';
}

# A single fake-infix adverb inside parens.
{
    my Pair $p;
    is-deeply ($p .= new :key<foo>), Pair.new(:key<foo>),
        'single parenthesized fake-infix adverb';
}

# Colon-arg form (no space before colon) still works inside parens.
{
    my @a;
    is-deeply (@a .= grep: { $_ > 1 }), [], 'colon-arg form still parses in parens';
}

# The whole construct works as an argument to another routine.
{
    my Pair $p;
    my @collected;
    @collected.push: ($p .= new :key<x> :value<y>);
    is-deeply @collected[0], :x<y>.Pair, 'parenthesized .= adverbs as routine argument';
}
