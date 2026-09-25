use Test;

# The six binary set operators have one body (#9451): the infix, the
# `[op]` reduction and the `&infix:<op>` routine forms must agree, and the
# result takes the left operand's shape.

my @p = (a => 2, "b", "b", c => 0);
my $bag = bag(<a a a b x>);

# Every operator reads a list of pairs as `.Bag` does: a Pair weighs its value.
my @cases =
    '∪', &infix:<∪>, (a => 3, b => 2, x => 1).Bag,
    '∩', &infix:<∩>, (a => 2, b => 1).Bag,
    '⊎', &infix:<⊎>, (a => 5, b => 3, x => 1).Bag,
    '⊍', &infix:<⊍>, (a => 6, b => 2).Bag,
    '∖', &infix:<∖>, (b => 1).Bag,
    '⊖', &infix:<⊖>, (a => 1, b => 1, x => 1).Bag;
for @cases -> $name, &op, $want {
    ok op(@p, $bag) eqv $want, "&infix:<$name> over a list of pairs";
    ok ([[&op]] @p, $bag) eqv $want, "[$name] over a list of pairs";
}
ok (@p ∪ $bag) eqv (a => 3, b => 2, x => 1).Bag, '∪ over a list of pairs';
ok (@p ∩ $bag) eqv (a => 2, b => 1).Bag, '∩ over a list of pairs';
ok (@p ∖ $bag) eqv (b => 1).Bag, '∖ over a list of pairs';
ok (@p ⊖ $bag) eqv (a => 1, b => 1, x => 1).Bag, '⊖ over a list of pairs';

# A Hash on the right of (-) weighs its values, like any other operand.
ok (bag(<a a a>) ∖ {a => True}) eqv (a => 2).Bag, 'Bag ∖ Hash subtracts the hash weights';
ok (bag(<a a a>) ∖ {a => 5}) eqv bag(), 'Bag ∖ Hash can empty a key';

# (==) reads its operands the same way.
ok (a => 2,) ≡ bag(<a a>), '(==) weighs a Pair';

# The result keeps the left operand's mutability and role mixin when it stays
# at the left operand's own type.
role R { method foo { 42 } }
my $q = (1, 2).SetHash but R;
is ($q ∪ set(3)).^name, 'SetHash+{R}', 'SetHash+{R} ∪ Set keeps the mixin';
is ($q ∩ set(1)).^name, 'SetHash+{R}', 'SetHash+{R} ∩ Set keeps the mixin';
is ($q ∖ set(1)).^name, 'SetHash+{R}', 'SetHash+{R} ∖ Set keeps the mixin';
is ($q ⊖ set(1)).^name, 'SetHash+{R}', 'SetHash+{R} ⊖ Set keeps the mixin';
is ($q ∪ set(3)).foo, 42, 'the kept mixin still answers its method';
is ($q ⊎ set(1)).^name, 'BagHash', 'SetHash+{R} ⊎ Set promotes to a fresh BagHash';
is ($q ∪ bag(1)).^name, 'BagHash', 'a promotion drops the mixin';
is ($q ⊖ (1, 3)).^name, 'Set', '⊖ with a non-QuantHash right operand demotes';
is ([∪] $q, set(3)).^name, 'SetHash+{R}', '[∪] keeps the mixin';
is (&infix:<∪>($q, set(3))).^name, 'SetHash+{R}', '&infix:<∪> keeps the mixin';
is (((1, 2, 2).BagHash but R) ⊎ bag(1)).^name, 'BagHash', 'BagHash+{R} ⊎ builds a fresh BagHash';
is (((1, 2, 2).BagHash but R) ⊍ bag(1)).^name, 'BagHash+{R}', 'BagHash+{R} ⊍ keeps the mixin';
is ((bag(1, 2) but R) ⊎ bag(1)).^name, 'Bag+{R}', 'Bag+{R} ⊎ keeps the mixin';
is (((1 => 0.5).MixHash but R) ∩ set(1)).^name, 'MixHash+{R}', 'MixHash+{R} ∩ keeps the mixin';

# A lazy operand is refused with the same error by every form.
for &infix:<∪>, &infix:<∩>, &infix:<⊖> -> &op {
    throws-like { op(1..*, set(1)) }, X::Cannot::Lazy, "{&op.name} refuses a lazy list";
}
throws-like { (1..*) ∪ set(1) }, X::Cannot::Lazy, '∪ refuses a lazy list';
throws-like { (1..*) ⊎ set(1) }, X::Cannot::Lazy, '⊎ refuses a lazy list';

done-testing;
