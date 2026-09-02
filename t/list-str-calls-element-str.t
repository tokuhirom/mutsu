use Test;

# Stringifying a LIST must call each element's own `Str`. mutsu's pure
# renderer (`to_string_value`) cannot dispatch a user method, so an Instance
# element rendered as the `ClassName()` fallback -- `~@a` and `@a.join("")`
# disagreed for the same array. Every string-context entry point now resolves
# the elements first and hands the result to the same renderer, so the
# list-shape rules (space separation, nested flattening) stay in one place.

plan 16;

class C { has $.t; method Str { $!t } }
class D { has $.t; method Stringy { "S:" ~ $!t } }

{
    my @a = (C.new(t => "hi"),);
    is @a.Str, 'hi', '.Str on a list';
    is ~@a, 'hi', 'prefix ~ on a list';
    is "x{@a}y", 'xhiy', 'interpolation of a list';
    is @a.join(","), 'hi', '.join still agrees';
    ok @a eq 'hi', 'infix eq on a list';
    is @a, 'hi', 'Test is() on a list';
}

{
    my @a = (1, C.new(t => "hi"), "z");
    is ~@a, '1 hi z', 'mixed elements are space-separated';
}

{
    my @a = ([C.new(t => "a"), C.new(t => "b")], C.new(t => "c"));
    is ~@a, 'a b c', 'nested lists flatten as usual';
}

{
    my $d = D.new(t => "q");
    # `.Str` and the list stringification path must not fall back to a
    # user-defined `Stringy`; only string context (`~`) does that.
    is ~($d,), ($d,).join(""), 'the list path agrees with .join';
    nok ~($d,) eq $d.Stringy, 'the list path uses .Str, not .Stringy';
}

# gist/raku are unchanged -- they are the object-inspection renderers.
{
    my @a = (C.new(t => "hi"),);
    is @a.gist, '[C.new(t => "hi")]', '.gist still shows the object';
    is @a.raku, '[C.new(t => "hi")]', '.raku still shows the object';
}

# A Seq / lazy list must stringify its elements the SAME way, or `is` compares
# one side through the element's `Str` and the other through the pure renderer
# (roast/integration/advent2009-day20.t's `is @b, (@people.sort: {...})`).
{
    my @a = (C.new(t => "a"), C.new(t => "b"));
    is @a, @a.Seq, 'a Seq stringifies its elements like an Array';
    is ~@a.Seq, 'a b', 'and prefix ~ on that Seq agrees';
    is @a, @a.map({ $_ }), 'a mapped Seq agrees too';
}

# A list with no such element takes the untouched fast path.
{
    is ~(1, 2, 3), '1 2 3', 'a plain list is unaffected';
}
