use Test;
use MONKEY-TYPING;

# The augment_class counterpart of t/method-custom-trait-typed-method-param.t:
# a Method-typed trait_mod:<is> candidate must also be invoked for a method
# declared inside `augment class`, matching the class walker.

plan 1;

my @applied;
multi sub trait_mod:<is>(Method $m, :$loud!) {
    @applied.push($m.name);
}

class AugTrait { }
augment class AugTrait {
    method greet() is loud { "hi" }
}

is @applied, ("greet",), 'a Method-typed trait_mod:<is> candidate is invoked for an augment-declared method trait';
