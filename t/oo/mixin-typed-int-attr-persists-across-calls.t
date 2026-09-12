use Test;

# #8047: a role mixed in at runtime (`but`/`does`) whose method mutates a
# NATIVE-typed attribute (`has int $.n`) via prefix increment used to write
# against a per-call copy and lose it, turning an exhaustion loop like
# `&each`'s cursor into an infinite one. Already fixed by the time this was
# investigated (measured against `t/oo/role/mixin-role-attribute-cell.t`'s
# non-native-int coverage); pinning the exact shapes from the issue so a
# regression is caught.

plan 9;

role Bumper { has int $.n; method bump() { ++$!n } }

class C { }
my $o = C.new but Bumper;
is $o.bump, 1, '`but` mixin: native int attribute starts at 1 after one bump';
is $o.bump, 2, 'and advances on a second bump';
is $o.bump, 3, 'and a third';

my @a = <a b c>;
@a does Bumper;
is @a.bump, 1, '`does` on an existing array variable: first bump is 1';
is @a.bump, 2, 'second bump is 2, not stuck at 1';
is @a.bump, 3, 'third bump is 3';

# The reduced &each shape: an INIT method writes a negative seed, and each
# `.each` call both reads and advances the same attribute.
role EachArray {
    has int $.index;
    method INIT() { $!index = -1; self }
    method each() {
        ++$!index < self.elems ?? ($!index, self.AT-POS($!index)) !! Empty
    }
}
my @b = <a b c>;
my $x := (@b does EachArray).INIT;
is-deeply $x.each, (0, 'a'), 'EachArray: INIT seed is visible to the first .each';
is-deeply @b.each, (1, 'b'), 'the second .each call advances past the first';
is-deeply @b.each, (2, 'c'), 'and the third reaches the last element';
