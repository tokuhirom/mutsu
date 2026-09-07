use v6;
use Test;

# `.^name` of a multi-role mixin renders one `+{...}` per APPLICATION, in the
# order the compositions happened:
#
#     ((1 but A) but B).^name    # Int+{A}+{B}
#     ((1 but B) but A).^name    # Int+{B}+{A}
#     (1 but (A, B)).^name       # Int+{A,B}   -- ONE application
#
# mutsu wrote `Int+{A,B}` for all three: `role_mixin_suffix_excluding` joined
# every composed role into a single bracket and sorted the names alphabetically
# ("HashMap iteration order is non-deterministic; sort for a stable name"),
# which threw away both the bracketing and the order.
#
# The same normalization reached the ADR-0060 composition key, so the two
# orderings shared one `.WHAT` cache node and `=:=` answered True where raku
# answers False. Order and grouping are part of the composed TYPE in raku, so
# the fix moves both together: the name, the `.WHAT` identity and the `===`
# identity key all order by the `__mutsu_role_seq__` stamp and group by the
# `__mutsu_role_group__` one, without either stamp's absolute value entering a
# key (two separately-built instances of the same composition must still share
# a `.WHAT`, and must still be `===` and `eqv`).
#
# Every expectation below was measured against rakudo 2026.07.

plan 20;

role A { }
role B { }
role C { }
role P[::T] { }

# --- the name: one bracket per application, in order -------------------
is (1 but A).^name, 'Int+{A}', 'a single role is unchanged';
is ((1 but A) but B).^name, 'Int+{A}+{B}', 'two sequential compositions get a bracket each';
is ((1 but B) but A).^name, 'Int+{B}+{A}', 'and the opposite order renders the other way';
is (((1 but A) but B) but C).^name, 'Int+{A}+{B}+{C}', 'three of them';
is (("s" but A) but B).^name, 'Str+{A}+{B}', 'over a Str base too';
is ((1 but P[Int]) but B).^name, 'Int+{P[Int]}+{B}',
    'a parameterised role keeps its arguments in its own bracket';

# --- one `but` over a LIST is ONE application ---------------------------
is (1 but (A, B)).^name, 'Int+{A,B}', 'a role LIST is one application, comma-joined';
is (1 but (B, A)).^name, 'Int+{B,A}', 'in written order';
is ((1 but (A, B)) but C).^name, 'Int+{A,B}+{C}', 'a list then a single role';
is ((1 but C) but (A, B)).^name, 'Int+{C}+{A,B}', 'and a single role then a list';

# --- the composed TYPE follows the same rule ---------------------------
{
    my $x = (1 but A) but B;
    my $y = (1 but B) but A;
    nok $x.WHAT =:= $y.WHAT, 'two orderings are different types';
    is $x.WHAT.^name, 'Int+{A}+{B}', 'and each .WHAT renders its own order';
    is $y.WHAT.^name, 'Int+{B}+{A}', '... both of them';
}
{
    my $a = 1 but (A, B);
    my $b = (1 but A) but B;
    nok $a.WHAT =:= $b.WHAT, 'one application of two roles is not two applications';
}

# --- what must NOT move: identical compositions stay identical ---------
{
    ok ((1 but A) but B).WHAT =:= ((1 but A) but B).WHAT,
        'two separately-built identical compositions share one .WHAT';
    ok (1 but (A, B)).WHAT =:= (1 but (A, B)).WHAT, '... list form too';
    ok ((1 but A) but B) === ((1 but A) but B), 'and they are ===';
    ok ((1 but A) but B) eqv ((1 but A) but B), 'and eqv';
    nok ((1 but A) but B) === ((1 but B) but A), 'while two orderings are not ===';
}

# A punned role is still named plainly, not `R+{R}`.
{
    role Q { }
    is Q.new.^name, 'Q', 'a punned role keeps its bare name';
}
