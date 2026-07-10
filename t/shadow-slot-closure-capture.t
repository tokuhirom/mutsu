use v6;
use Test;

# §1.3 closure-capture slot bake: a closure over a lexical that SHADOWS an
# enclosing same-named binding must capture the inner (shadow) slot's cell,
# even when the closure does not escape by the compiler's escape analysis
# (e.g. passed as a call argument). Pin for S06-advanced/wrap.t tests 63/66.
# Must pass with MUTSU_SHADOW_SLOTS unset (default) AND =1, and on real raku.

plan 8;

my $w = "outer-w";
{
    my $w;
    my @cs;
    @cs.push({ $w = "set-w"; });
    @cs[0]();
    is $w, "set-w", 'non-escaping closure (call arg) writes the inner shadow';
}
is $w, "outer-w", 'outer binding untouched by the inner-shadow write';

# wrap.t shape: wrapper closure created in a for-loop body, invoked later
# from the wrapped sub's call frame.
my $wrapped = "outer-wrapped";
{
    sub meet($person)  { "meet $person" }
    sub greet($person) { "greet $person" }
    my $wrapped;
    for &greet, &meet -> $wrap {
        my $name = $wrap.name;
        $wrap.wrap({ $wrapped = $name; callsame; });
    }
    is greet('a'), 'greet a', 'wrapped sub still works';
    is $wrapped, 'greet', 'wrapper wrote the shadowed lexical (greet)';
    is meet('b'), 'meet b', 'second wrapped sub still works';
    is $wrapped, 'meet', 'wrapper wrote the shadowed lexical (meet)';
}
is $wrapped, "outer-wrapped", 'outer binding untouched by the wrappers';

# Closure created BEFORE the shadow block captures the OUTER slot: the baked
# emit-point slot must win over an innermost-name search.
my $x = 1;
my $c = { $x = 5; };
{ my $x = 3; }
$c();
is $x, 5, 'pre-shadow closure writes the outer slot, not the later shadow';
