use Test;

# A `sub h` a block or an `EVAL` declares is what a bare `h()` there calls,
# even when an outer scope binds a `my &h`; the outer binding is unchanged
# afterwards (#13855). Checked against rakudo.
plan 7;

my &h = sub { 'outer' };
my @got;
{ sub h { 'inner' }; @got.push: h() }
is @got[0], 'inner', 'a mainline block sub shadows the outer my &h';
is h(), 'outer', 'the outer binding is unchanged after the block';

{ sub h($x) { "inner-$x" }; @got.push: h(5) }
is @got[1], 'inner-5', 'with arguments';

my &k = sub { 'outer-k' };
is EVAL(q[sub k { 'eval' }; k()]), 'eval', "an EVAL's sub shadows the caller's my &k";
is k(), 'outer-k', "the caller's binding is unchanged after the EVAL";

if True { sub h { 'if-inner' }; @got.push: h() }
is @got[2], 'if-inner', 'an if body sub';

# The block is the unit's last statement: it runs as an inlined block frame,
# where the declaration is not a redeclaration of the outer `&h`.
{ sub h { 'tail-inner' }; is h(), 'tail-inner', 'a block that ends the unit' }
