use Test;

plan 12;

# The pseudo-methods `.WHAT`, `.WHO`, `.HOW`, `.VAR`, `.REPR`, `.DEFINITE`
# and `.WHERE` never prime: after a priming chain (`*.abs`) they apply to the
# finished WhateverCode instead of joining its body (#18514).

is (*.abs.WHAT).raku, 'WhateverCode', '*.abs.WHAT is the WhateverCode type';
is ((*).abs.WHAT).raku, 'WhateverCode', '(*).abs.WHAT likewise';
is ((* + 1).abs.WHAT).raku, 'WhateverCode', '(* + 1).abs.WHAT likewise';
is *.abs.WHO, 'WhateverCode', '*.abs.WHO';
is *.abs.HOW.^name, 'Perl6::Metamodel::ClassHOW', '*.abs.HOW';
is *.abs.VAR.^name, 'WhateverCode', '*.abs.VAR';
is *.abs.REPR, 'P6opaque', '*.abs.REPR';
is *.REPR, 'P6opaque', '*.REPR answers for the Whatever';
is *.abs.DEFINITE, True, '*.abs.DEFINITE';
is (*.WHAT).raku, 'Whatever', '*.WHAT is still the Whatever type';

# `.^name` and `.WHICH` do prime.
isa-ok *.abs.^name, WhateverCode, '*.abs.^name primes';
my &f = *.abs;
is f(-3), 3, 'the priming chain itself is unchanged';
