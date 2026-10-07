use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# A WhateverCode `where` clause in a `:(...)` literal survives `.AST` -> EVAL.
# The literal is a value, so the lowered program's curry pass cannot reach the
# parameters inside it; the FakeSignature lowering curries them itself.

plan 6;

my $a = EVAL Q[:(Str $x where *.ends-with('.html'))].AST;
ok $a.ACCEPTS(\('b.html')), 'WhateverCode method where accepts';
nok $a.ACCEPTS(\('b.txt')), 'WhateverCode method where rejects';

my $d = EVAL Q[:(*@p where *.elems == 2)].AST;
ok $d.ACCEPTS(\('f', 'b.html')), 'slurpy WhateverCode where accepts';
nok $d.ACCEPTS(\('f')), 'slurpy WhateverCode where rejects';

my $c = EVAL Q[:(*@p where { .elems == 2 })].AST;
ok $c.ACCEPTS(\('f', 'b.html')), 'block where accepts';
nok $c.ACCEPTS(\('f')), 'block where rejects';
