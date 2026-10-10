use AttrDefaultClosureHelper;
unit class AttrDefaultClosureHolder;
has &.p = -> $v { twice-it($v) };
has $.q = twice-it(4);
has &.r = -> { -> $v { twice-it($v) } };
method run($v) { &!p($v) }
