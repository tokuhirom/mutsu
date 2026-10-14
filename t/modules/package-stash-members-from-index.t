use Test;

# A named package's stash is built from the names the package index lists
# (#9171), not from a scan of the whole env, `our` store and routine registry.
# Every expectation was checked against rakudo.

plan 7;
package P { our $x = 1; our sub f { 2 }; our @a = 1, 2; class C { }; package Q { our $y = 3 } }
is P::.keys.sort.join(' '), '$x &f @a C Q', 'a named package lists its members';
$P::z = 4;
is P::.keys.sort.join(' '), '$x $z &f @a C Q', 'a member assigned from outside the package';
{ is P::.keys.sort.join(' '), '$x $z &f @a C Q', 'the same stash read inside a block'; }
sub g { P::.keys.sort.join(' ') }
is g(), '$x $z &f @a C Q', 'the same stash read inside a sub';
is P::Q::.keys.join(' '), '$y', 'a nested package lists its own members';
module M { our proto sub pr(|) {*}; our multi sub pr(Int) { 1 }; class D { } }
is M::.keys.sort.join(' '), '&pr D', 'a proto routine and a class';
is P::<&f>(), 2, 'a member read through the stash';
