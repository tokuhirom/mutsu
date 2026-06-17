use Test;

# Set operators take their result mutability from the FIRST operand, and
# `eqv` distinguishes Set from SetHash (and Bag from BagHash, Mix from MixHash).

plan 25;

# eqv distinguishes mutability
nok Set.new(42) eqv SetHash.new(42), 'Set does not eqv SetHash';
ok  Set.new(42) eqv Set.new(42), 'Set eqv Set';
ok  SetHash.new(42) eqv SetHash.new(42), 'SetHash eqv SetHash';
nok Bag.new(<a a>) eqv BagHash.new(<a a>), 'Bag does not eqv BagHash';
nok Mix.new(<a a>) eqv MixHash.new(<a a>), 'Mix does not eqv MixHash';

# union/intersection/difference: result follows the left operand
is (SetHash.new(<a b>) (|) Set.new(<c>)).^name, 'SetHash', '(|) SetHash left -> SetHash';
is (Set.new(<a b>) (|) SetHash.new(<c>)).^name, 'Set', '(|) Set left -> Set';
is (SetHash.new(<a b>) (&) SetHash.new(<a>)).^name, 'SetHash', '(&) SetHash -> SetHash';
is (Set.new(<a b>) (&) SetHash.new(<a>)).^name, 'Set', '(&) Set left -> Set';
is (SetHash.new(<a b>) (-) Set.new(<a>)).^name, 'SetHash', '(-) SetHash left -> SetHash';
is (Set.new(<a b>) (-) SetHash.new(<a>)).^name, 'Set', '(-) Set left -> Set';

# addition/multiply promote to Bag level, keeping left mutability
is (SetHash.new(<a b>) (+) Set.new(<a>)).^name, 'BagHash', '(+) SetHash left -> BagHash';
is (Set.new(<a b>) (+) SetHash.new(<a>)).^name, 'Bag', '(+) Set left -> Bag';
is (BagHash.new(<a b>) (.) Bag.new(<a>)).^name, 'BagHash', '(.) BagHash left -> BagHash';
is (Bag.new(<a b>) (.) BagHash.new(<a>)).^name, 'Bag', '(.) Bag left -> Bag';

# symmetric difference: binary form stays mutable only when BOTH are QuantHashes
is (SetHash.new(<a b>) (^) Set.new(<b c>)).^name, 'SetHash', '(^) SetHash (^) Set -> SetHash';
is (SetHash.new(<a b>) (^) <b c>).^name, 'Set', '(^) SetHash (^) List -> Set';
is (Set.new(<a b>) (^) SetHash.new(<b c>)).^name, 'Set', '(^) Set left -> Set';

# 3+-arg symmetric difference follows the first operand
is (&infix:<(^)>(SetHash.new(<a>), SetHash.new(<b>), <c>)).^name, 'SetHash',
    '3-arg (^) SetHash first -> SetHash';
is (&infix:<(^)>(Set.new(<a>), SetHash.new(<b>), SetHash.new(<c>))).^name, 'Set',
    '3-arg (^) Set first -> Set';

# single-arg reductions
is (&infix:<(|)>(SetHash.new(<a b>))).^name, 'SetHash', 'single (|) preserves SetHash';
is (&infix:<(-)>(SetHash.new(<a b>))).^name, 'Set', 'single (-) demotes to Set';

# eqv on the results of operators
ok (SetHash.new(<a b>) (|) SetHash.new(<c>)) eqv SetHash.new(<a b c>),
    'SetHash union eqv SetHash';
nok (Set.new(<a b>) (|) Set.new(<c>)) eqv SetHash.new(<a b c>),
    'Set union does not eqv SetHash';
ok (Set.new(<a b>) (|) SetHash.new(<c>)) eqv Set.new(<a b c>),
    'mixed union (Set left) eqv Set';
