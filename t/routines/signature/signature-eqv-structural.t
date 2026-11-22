use Test;

# #18679: `Signature eqv Signature` is structural, as Rakudo's `infix:<eqv>`:
# a positional parameter's name does not count, a named one's does.

plan 20;

ok :(Int $x) eqv :(Int $y), 'positional names do not count';
ok :($a, $b) eqv :($c, $d), 'two untyped positionals';
ok :(*@a) eqv :(*@b), 'slurpies';
ok :(Int $x --> Str) eqv :(Int $y --> Str), 'the same return type';
nok :(Int $x --> Str) eqv :(Int $y), 'a different return type';
nok :(Int $x) eqv :(Str $x), 'a different type';
nok :($a, $b) eqv :($c), 'a different arity';
nok :(:$a) eqv :(:$b), 'named names count';
ok :(:$a, :$b) eqv :(:$b, :$a), 'nameds match in any order';
nok :(:$a!) eqv :(:$a), 'required differs from optional';
nok :($a?) eqv :($b), 'optional differs from required';
ok :($x is rw) eqv :($y is rw), 'the same trait';
nok :($x is rw) eqv :($y), 'a different trait';
ok :([$a, $b]) eqv :([$c, $d]), 'sub-signatures compare structurally';
nok :($x where * > 0) eqv :($y where * > 0), 'a where block never matches';
ok :(0) eqv :(0), 'the same literal';
nok :(0) eqv :(1), 'a different literal';
nok :(@a) eqv :(%h), 'a different sigil';
nok :(**@a) eqv :(*@b), 'a different slurpy kind';

sub f(Int $a) {}
sub g(Int $b) {}
ok &f.signature eqv &g.signature, "routines' signatures";
