use v6;
use Test;

plan 8;

# `Signature ~~ Signature` is always False when the *matcher* (RHS) has a
# `where` clause: it carries arbitrary runtime code that cannot be statically
# proven to accept a candidate signature (Rakudo).
nok :(42) ~~ :($ where 42), 'literal vs where-matcher does not match';
nok :(Int) ~~ :($ where Int), 'type vs where-matcher does not match';
nok :($ where 42) ~~ :($ where 42), 'identical where signatures still do not match';
nok :($x) ~~ :($ where 42), 'plain param vs where-matcher does not match';

# Signatures without a `where` on the matcher compare structurally as before.
ok  :(42) ~~ :(42), 'identical literal signatures match';
ok  :(42) ~~ :(Int), 'a literal is accepted by a matching type';
ok  :($a, $b) ~~ :($x, $y), 'same-arity plain signatures match';

# A `where` on the LHS (not the matcher) is unaffected by this rule; the match
# is decided by the structural comparison (here: arity/type mismatch).
nok :($ where 42) ~~ :(Int, Int), 'lhs where, arity mismatch is still False';

done-testing;
