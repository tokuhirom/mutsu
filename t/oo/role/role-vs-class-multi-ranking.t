use Test;
plan 12;

# Ranking multi candidates asks whether one declared type conforms to another;
# a role as the narrower side is answered from the role's ancestors (its role
# parents, the roles it composes) rather than a fresh type-hierarchy walk per
# comparison. The answers must stay those of the general walk (debug builds
# assert it on every query).
role Shape { }
role Round does Shape { }
role Solid { }
class Base { }
class Ball does Round { }
class Block is Base { }
class Slab does Solid { }

multi sub describe(Shape $x) { "shape" }
multi sub describe(Round $x) { "round" }
multi sub describe(Base $x) { "base" }
multi sub describe(Solid $x) { "solid" }
multi sub describe(Any $x) { "any" }

is describe(Ball.new), 'round', 'a role that does a role is narrower than the parent role';
is describe(Block.new), 'base', 'a class constraint is picked for its instance';
is describe(Slab.new), 'solid', 'an unrelated role constraint is picked for the class that does it';
is describe(Ball), 'round', 'a type object ranks the same way';
is describe(42), 'any', 'an unrelated value falls to Any';

# A role and a class that are unrelated stay unrelated, in both directions.
multi sub pick(Round $x, Base $y) { "round+base" }
multi sub pick(Shape $x, Any $y) { "shape+any" }
is pick(Ball.new, Block.new), 'round+base', 'role vs class candidates are ranked per argument';
is pick(Ball.new, 5), 'shape+any', 'the wider role candidate takes the unrelated class case';

# A role added later is seen: the ancestor sets follow the registry.
role Later does Shape { }
class Late does Later { }
multi sub describe(Later $x) { "later" }
is describe(Late.new), 'later', 'a role declared after the first dispatches ranks correctly';
is describe(Ball.new), 'round', 'and earlier answers are unchanged';

# A role with a parent class conforms to it.
role Wheeled is Base { }
multi sub kind(Wheeled $x) { "wheeled" }
multi sub kind(Base $x) { "base" }
class Cart does Wheeled { }
is kind(Cart.new), 'wheeled', 'a role is narrower than the class it inherits';
is kind(Base.new), 'base', 'the class itself still takes its own candidate';
is describe(Cart.new), 'base', 'through its role parent a Cart is a Base';
