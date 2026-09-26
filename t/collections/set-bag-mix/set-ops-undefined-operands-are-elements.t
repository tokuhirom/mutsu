use Test;

# #9481: set operators and `.Set` dropped an `Any`, `Nil` or `""` operand
# instead of taking it as an element. That was the old "uninitialized-scalar
# seed" rule leaking out of `$s ∪= x`. In rakudo the seed comes from
# METAOP_ASSIGN, which replaces an undefined left side with the operator's
# zero-argument value (`set()` / `bag()`); the operators themselves drop
# nothing.

plan 17;

# --- the operators take undefined and empty operands as elements ------------
is-deeply (Any ∪ set(1)), Set.new(1, Any), 'Any is an element of a union';
is-deeply ("" ∪ set(1)), Set.new(1, ""), 'so is the empty string';
# (Not `Set.new(1, Nil)`: a Nil argument to `.new` binds as Any.)
is-deeply (Nil ∪ set(1)).keys.map(*.raku).sort, ("1", "Nil"), 'and so is Nil';
is-deeply (Any (+) bag(1)), (Any, 1).Bag, 'a Bag operator counts Any too';
is-deeply ("", "a").Set, Set.new("", "a"), '.Set keeps an empty string';
is-deeply (Nil, "a").Set.keys.map(*.raku).sort, ("\"a\"", "Nil"), '.Set keeps Nil';
is-deeply Any.Set, Set.new(Any), 'Any.Set is a one-element Set';

# --- an undefined left side of `op=` starts from the identity ----------------
{
    my $s; $s ∪= 0;
    is-deeply $s, Set.new(0), '∪= on an undefined scalar unions from set()';
    my $t; $t ∩= set(1, 2);
    is-deeply $t, set(), '∩= intersects with set()';
    my $u; $u (-)= set(1);
    is-deeply $u, set(), '(-)= subtracts from set()';
    my $n = Nil; $n (^)= set(4);
    is-deeply $n, Set.new(4), '(^)= on a Nil-assigned scalar';
    my $b; $b (+)= bag(1);
    is-deeply $b, bag(1), '(+)= adds to bag()';
    my $m; $m (.)= bag(1, 1);
    is-deeply $m, bag(), '(.)= multiplies with bag()';
}

# --- every lvalue form gets the same seed ------------------------------------
{
    my %h; %h<k> ∪= 1;
    is-deeply %h<k>, Set.new(1), 'a hash element';
    my @a; @a[0] (+)= bag(2, 2);
    is-deeply @a[0], bag(2, 2), 'an array element';
    my Set $ts; $ts ∪= 3;
    is-deeply $ts, Set.new(3), 'a Set-typed scalar';
    my class O { has $.s; method add($v) { $!s ∪= $v } }
    my $o = O.new; $o.add(7);
    is-deeply $o.s, Set.new(7), 'an attribute';
}
