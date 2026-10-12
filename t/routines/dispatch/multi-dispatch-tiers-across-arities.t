use Test;

# A bare-name multi call ranks every candidate of the name, in every arity,
# in one set of rakudo topological tiers (#12795). The resolver used to try
# the exact-arity candidates on their own first, so a narrower candidate with
# an optional parameter (registered under another arity) could not compete
# with them. Expected values from rakudo.

plan 15;

sub t(&c) { my $r = try c(); $r // ($! ?? $!.^name !! 'Nil') }

# `(Int $x, $y?)` is narrower than `(Cool $x)` on the one shared position.
multi sub a(Cool $x)        { 'exact-Cool' }
multi sub a(Int $x, $y?)    { 'opt-Int' }
is a(1),   'opt-Int',    'a narrower optional-arity candidate beats an exact-arity one';
is a('s'), 'exact-Cool', 'the exact-arity one still wins what only it binds';

multi sub b(Int $x, $y?)    { 'opt-Int' }
multi sub b(Cool $x)        { 'exact-Cool' }
is b(1), 'opt-Int', 'whatever the declaration order';

multi sub h(Str $x)               { 'exact-Str' }
multi sub h(Int $x, Int $y = 2)   { 'opt-Int' }
is h(1),   'opt-Int',   'a defaulted parameter competes as well';
is h('x'), 'exact-Str', 'and the exact candidate keeps its own calls';

# An optional positional the call does not reach takes no part in the
# comparison, so `()` is no wider than `(Int $a?)` for a call with no
# arguments, and the one without optionals wins.
multi sub c(Int $a?) { 'Int' }
multi sub c()        { 'zero' }
is c(),  'zero', 'the zero-arity candidate wins a call with no arguments';
is c(5), 'Int',  'the optional one takes an argument';

# A capture with a sub-signature is a slurpy capture with a bind check:
# narrower than a slurpy array, wider than a plain positional.
multi sub d(|c($x)) { 'capture' }
multi sub d($n)     { 'plain' }
is d(1), 'plain', 'a plain positional beats a sub-signature capture';

multi sub e($n)     { 'plain' }
multi sub e(|c($x)) { 'capture' }
is e(1), 'plain', 'whatever the declaration order';
is t({ e(1, 2) }), 'X::Multi::NoMatch', 'the capture still checks its sub-signature';

multi sub f(*@a)     { 'slurpy' }
multi sub f(|c($x))  { 'capture' }
is f(1), 'capture', 'a sub-signature capture beats a slurpy array';

# Arity mismatch: a candidate without a slurpy is narrower than one with it.
multi sub g(Numeric $x, Numeric $y) { 'two' }
multi sub g(Int $x, *@r)            { 'slurpy' }
is g(1, 2), 'two', 'a fixed-arity candidate beats a slurpy one of another arity';
is g(1), 'slurpy', 'the slurpy one takes the call only it binds';

multi sub i(Any $x)            { 'exact-Any' }
multi sub i(Int $x, :$n!)      { 'named' }
multi sub i(Int $x, $y?)       { 'opt' }
is i(1), 'opt', 'a required-named rival does not hide the optional-arity winner';
is i(1, :n(2)), 'named', 'the required-named candidate wins when its named is passed';
