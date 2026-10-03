use Test;

# A package-qualified routine called as a listop takes an argument that opens
# with a glued prefix operator: `M::dec ~$s` is `M::dec(~$s)`, as in Rakudo.
# It used to parse as `M::dec() ~ $s`, so the call died for want of an
# argument (Bitcoin's `checkedB58Str` subset calls `Base58::decode ~$/`
# inside a regex code assertion).

plan 8;

module M {
    our sub dec($x) { "got:$x" }
    our constant c = 5;
    our sub f() { 10 }
}

my $s = "abc";
is M::dec(~$s), 'got:abc', 'parenthesized call (baseline)';
is (M::dec ~$s), 'got:abc', 'listop call with a ~ prefixed argument';
is (M::dec -1), 'got:-1', 'listop call with a - prefixed argument';
is (M::dec +"5"), 'got:5', 'listop call with a + prefixed argument';

ok "abc" ~~ / ^ \w+ $ <?{ M::dec(~$/) eq 'got:abc' }> /, 'parenthesized inside a code assertion';
ok "abc" ~~ / ^ \w+ $ <?{ (M::dec ~$/) eq 'got:abc' }> /, 'listop inside a code assertion';

# A spaced infix is still an infix.
is M::c - 1, 4, 'a qualified constant minus a number is a subtraction';
is M::f() - 1, 9, 'a parenthesized call minus a number is a subtraction';
