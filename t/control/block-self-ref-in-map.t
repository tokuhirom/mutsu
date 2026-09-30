use Test;

# &?BLOCK must be the block itself inside a .map / .first callback.
# Found via TOML::Thumb's suite: `.dir».&?BLOCK` inside `.map: { ... }`.

plan 6;

is (3,).map({ &?BLOCK.WHAT.^name }).join, 'Block', '&?BLOCK is a Block in a map callback';

# Recursive flattening through &?BLOCK, as TOML::Thumb's t/valid.t does.
my @tree = (1, (2, (3, 4)), 5);
is @tree.map({ $_ ~~ List ?? |.map(&?BLOCK) !! $_ }).join(','), '1,2,3,4,5',
    'recursive map through &?BLOCK flattens nested lists';

is (3,).map({ $_ > 1 ?? ($_ - 1).&?BLOCK !! $_ }).raku, '(1,).Seq',
    'method-style .&?BLOCK recursion in map';

is (3,).map({ $_ > 1 ?? (&?BLOCK)($_ - 2) !! $_ }).raku, '(1,).Seq',
    '&?BLOCK invoked with an argument in map';

is (5, 6, 7).first({ &?BLOCK.WHAT.^name eq 'Block' && $_ > 5 }), 6,
    '&?BLOCK is visible in a first matcher';

is (1, 2, 3).grep({ &?BLOCK.defined && $_ > 1 }).join(','), '2,3',
    '&?BLOCK is defined in a grep matcher';
