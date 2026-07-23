use v6;
use Test;

plan 16;

# PLAN 8.5 step 4: the Nil value and the Any type object are distinct.
# The types_eqv "Nil eqv Package(Any)" crutch is gone; these pin the
# real semantics (all raku-verified).

is Nil.^name, 'Nil', 'Nil.^name is Nil';
ok Nil.WHAT === Nil, 'Nil.WHAT is the Nil type object';
is Nil.WHAT.raku, 'Nil', 'Nil.WHAT.raku is Nil';

nok Nil === Any, 'Nil === Any is False';
nok Any === Nil, 'Any === Nil is False';
ok Nil === Nil, 'Nil === Nil is True';

nok Nil eqv Any, 'Nil eqv Any is False';
nok Any eqv Nil, 'Any eqv Nil is False';
ok Nil eqv Nil, 'Nil eqv Nil is True';

ok Nil ~~ Any, 'Nil ~~ Any is True (Nil isa Any)';
ok Nil ~~ Mu, 'Nil ~~ Mu is True';
nok Any ~~ Nil, 'Any ~~ Nil is False';

# An uninitialized scalar still holds Any (not Nil) — the step-3 seed.
my $x;
ok $x === Any, 'uninit my $x holds the Any type object';

# The anonymous scalar `$` is a declared uninitialized scalar: Any.
is ($).WHAT.gist, '(Any)', 'anonymous $ variable reads as Any';

# A user subclass of Array/Hash defaults missing elements to Any,
# like its base container (S12-introspection/WHAT.t).
my class NA is Array {};
my class NH is Hash {};
ok NA.new[0].WHAT === Any, 'Array-subclass missing element is Any';
ok NH.new<k>.WHAT === Any, 'Hash-subclass missing value is Any';
