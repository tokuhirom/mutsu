use Test;

# #18698: a parenthesized `*` as an infix or prefix operand is the Whatever
# *value*, not a priming point (rakudo); a method-call or subscript target
# still primes through one layer of parentheses.

plan 10;

throws-like { (*) + 1 }, X::Multi::NoMatch, '(*) + 1 adds a Whatever';
throws-like { 1 + (*) }, X::Multi::NoMatch, '1 + (*) too';
throws-like { -(*) }, X::Multi::NoMatch, 'a prefix operand';
isa-ok ((*) ~ "a"), Str, '(*) ~ "a" stringifies the Whatever';
isa-ok ((*) eq "x"), Bool, '(*) eq "x" compares it';
isa-ok ((*) ~~ Int), Bool, '(*) ~~ Int smartmatches it';
isa-ok ((*) + *), WhateverCode, 'a bare * beside it still curries';
isa-ok (* + 1), WhateverCode, '* + 1 curries';
isa-ok ((*).abs), WhateverCode, 'a method call target primes through the parens';
isa-ok ((*)[0]), WhateverCode, 'so does a subscript target';
