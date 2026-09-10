use Test;

# `&.name` is the public accessor of a `has &.name` callable attribute — i.e.
# `self.name` — mirroring `$.attr`/`@.attr`/`%.attr`. mutsu parsed the scalar/
# array/hash `.`-twigil accessors but not the `&` one, so `&.function.($x)`
# failed to parse. Found via the real-distribution compat sweep (Metropolis,
# docs/dist-compat-sweep.md), which calls `&.function.( $!x )`.

plan 6;

{
    class C { has &.f; method run($x) { &.f.($x) } }
    is C.new(f => *+1).run(5), 6, '&.attr.($x) invokes the callable attribute';
}
{
    class C { has &.f = *+1; method m { &.f.(10) } }
    is C.new.m, 11, '&.attr with a default callable';
}
{
    class C { has &.jumper; method m { &.jumper.(mean => 5) } }
    is C.new(jumper => -> :$mean { $mean }).m, 5, '&.attr.(named => v) passes named args';
}
{
    # &.attr returns the callable itself (bare, no call)
    class C { has &.f; method getit { &.f } }
    my $c = C.new(f => { 42 });
    is $c.getit.(), 42, '&.attr returns the Callable, invocable later';
}
{
    # hyphenated attribute name
    class C { has &.my-fn; method m { &.my-fn.(3) } }
    is C.new(my-fn => *²).m, 9, '&.attr with a hyphenated name';
}
{
    # $.attr accessor for a & attribute still works too
    class C { has &.f = *+2; method m { $.f.(8) } }
    is C.new.m, 10, '$.attr accessor for a callable attribute still works';
}
