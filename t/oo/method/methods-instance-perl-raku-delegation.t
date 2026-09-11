use Test;

# `Mu.perl` is rakudo's deprecated spelling of `.raku`, implemented by CALLING
# `self.raku` — so a class that overrides only `raku` renders through that
# override under either name. mutsu resolved the two names independently and fell
# through to its own attribute-dump renderer for `.perl`, which broke
# Math::Vector's `multi method raku()` (it renders nested vectors with
# `@.components.map({.perl})`, and the result must round-trip through EVAL).

plan 6;

class Vec {
    has @.components;
    multi method new(*@x) { self.bless(components => @x) }
    multi method raku() { 'Vec.new(' ~ @.components.map({ .perl }).join(', ') ~ ')' }
}

my $v = Vec.new(1, 2, 3);
is $v.raku, 'Vec.new(1, 2, 3)', 'the user raku override renders';
is $v.perl, 'Vec.new(1, 2, 3)', '.perl reaches the user raku override';

# Nested: the inner element is reached through `.perl` inside the override.
my $nested = Vec.new(Vec.new(1, 2), Vec.new(3, 4));
is $nested.perl, 'Vec.new(Vec.new(1, 2), Vec.new(3, 4))',
   'a nested .perl recurses into the override';
is EVAL($nested.raku).raku, $nested.raku, 'the rendering round-trips through EVAL';

# An explicit `perl` override still wins, and does NOT get redirected to `raku`.
class Both {
    has $.n;
    method raku() { 'RAKU' }
    method perl() { 'PERL' }
}
is Both.new(n => 1).perl, 'PERL', 'an explicit perl override is untouched';

# A class with neither keeps the default rendering.
class Plain { has $.n }
is Plain.new(n => 1).perl, 'Plain.new(n => 1)', 'the default rendering is unchanged';
