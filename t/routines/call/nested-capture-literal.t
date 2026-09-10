use Test;

# A Capture literal nests like any other value: `\(1, \(2,3))` is a
# two-element Capture whose second element is itself a Capture. mutsu used to
# flatten every Capture element while BUILDING a capture, on the theory that it
# could only have come from a `|` slip -- but `|` compiles to its own opcode, so
# the only Captures reaching that arm were genuine nested literals. XML's
# `make-xml('rss', \('channel', \('title', 'x')))` built one flat element
# instead of a tree.

plan 10;

{
    is \(1, \(2, 3)).raku, '\\(1, \\(2, 3))', 'a nested capture literal keeps its nesting';
    is \(1, \(2, 3)).elems, 2, 'and counts as two positionals';
    is \('a', \('b', \('c', 'd'))).raku, '\\("a", \\("b", \\("c", "d")))',
        'three levels nest too';
}

# `|` still interpolates -- that is the spelling for flattening one in.
{
    my $c = \(2, 3);
    is \(1, |$c).raku, '\\(1, 2, 3)', '|$capture flattens into the outer capture';
    my $d = \(:a<x>);
    is \(1, |$d).raku, '\\(1, :a("x"))', 'and carries its named lane over';
}

# A nested Capture survives being passed through a slurpy.
{
    sub probe(Str $n, *@c, *%a) { ($n, @c.raku, %a.raku).join(' | ') }
    is probe('rss', \('channel', \('title', 'x'))),
        'rss | [\\("channel", \\("title", "x"))] | {}',
        'a slurpy sees the nested capture whole';
    sub relay(Str $n, *@contents, *%attribs) { probe($n, |@contents, |%attribs) }
    is relay('rss', :v<yes>, \('channel', \('title', 'x'))),
        'rss | [\\("channel", \\("title", "x"))] | {:v("yes")}',
        'and so does a re-splatted slurpy';
}

# A Capture element is a Capture, so `~~ Capture` sees it (this is how
# XML::Element!craft-new decides to recurse into a child element).
{
    my @parts = \(1, \(2, 3)).list;
    is @parts.elems, 2, 'the positional lane has both elements';
    ok @parts[1] ~~ Capture, 'and the second one smartmatches Capture';
    is @parts[1].raku, '\\(2, 3)', 'with its own contents intact';
}
