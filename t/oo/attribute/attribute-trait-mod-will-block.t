use v6;
use Test;

# Attribute::Lazy 0.0.7 uses the `will lazy { ... }` attribute-trait form.
# Rakudo passes the block positionally and the trait name as :lazy.

plan 4;

my %seen;

multi sub trait_mod:<will>(Attribute:D $attr, Callable $block, :$lazy!) {
    %seen<name> = $attr.name;
    %seen<block> = $block;
    %seen<lazy> = $lazy;
}

class Example {
    has $.value will lazy { 42 };
}

ok %seen<name> eq '$!value', 'will trait receives the Attribute object';
isa-ok %seen<block>, Callable, 'will trait receives its block as a Callable';
is %seen<block>(), 42, 'will trait preserves the block body';
ok %seen<lazy>, 'will trait receives its name as a named argument';

done-testing;
