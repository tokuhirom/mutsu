use Test;

# A block whose only statement is a `*`-curry is an ordinary block that returns
# the WhateverCode; only calling it on the spot is "Malformed double closure"
# (rakudo's RakuAST frontend). Identical under rakudo.
plan 8;

my $b = { * + 1 };
isa-ok $b, Block, 'a block of a curry stays a Block';
isa-ok $b.(), WhateverCode, 'and returns the WhateverCode';
is $b.()(2), 3, 'which works';
isa-ok (do { * + 1 }), WhateverCode, 'a do block is the WhateverCode';
multi sub infix:<⪪>(Mu $l, Mu $r) { "$l/$r" }
my $q = { $_ ⪪ * ⪪ 1 };
isa-ok $q, Block, 'a user infix with a Whatever operand in a block';

throws-like { EVAL '{ * + 1 }()' }, X::Syntax::Malformed, 'calling it with () is malformed';
throws-like { EVAL '{ *.abs }.()' }, X::Syntax::Malformed, 'calling it with .() is malformed';
throws-like { EVAL '({ *.abs })(1)' }, X::Syntax::Malformed, 'also through parentheses';
