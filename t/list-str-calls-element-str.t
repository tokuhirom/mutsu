use Test;

# Stringifying a LIST must call each element's own `Str`. mutsu's pure
# renderer (`to_string_value`) cannot dispatch a user method, so an Instance
# element rendered as the `ClassName()` fallback -- `~@a` and `@a.join("")`
# disagreed for the same array. Every string-context entry point now resolves
# the elements first and hands the result to the same renderer, so the
# list-shape rules (space separation, nested flattening) stay in one place.

plan 19;

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

# ... and infix `eq` must agree with all of the above. A Seq that still holds a
# deferred source reaches string context through the operand coercion, which
# reified it and RETURNED, skipping the element-`Str` resolution the Array side
# had already had -- so the two sides of an otherwise identical comparison were
# rendered by different stringifiers. These assertions fail through `eq`
# directly, so they catch it under mutsu's native Test provider too, not only
# under the vendored `Test.rakumod` whose `is` is the `eq` in question.
{
    my @a = (C.new(t => "a"), C.new(t => "b"));
    ok @a.Seq eq 'a b', 'infix eq on a Seq of objects uses the element Str';
    ok @a.Seq eq @a, 'a Seq compares eq to the same Array';
    ok @a.map({ $_ }) eq @a, 'a mapped Seq does too';
}

# A list with no such element takes the untouched fast path.
{
    is ~(1, 2, 3), '1 2 3', 'a plain list is unaffected';
}
