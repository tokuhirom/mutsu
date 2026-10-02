use v6;
use Test;

# Whether a user `multi infix:<op>` candidate out-ranks the operator's core
# candidate set is memoized per (operator, candidate, operand type keys)
# (#10111). The memo must answer exactly what ranking from scratch answers,
# across repeated calls, across calls whose operand types or definedness
# differ, and for a value-dependent candidate whose winner changes with the
# operand values. Every expectation below was checked against rakudo.

plan 9;

{
    multi infix:<+>(Int $a, Numeric $b) { "user" }
    # The core `(Int:D, Int:D)` and `(Real, Real)` rows both out-narrow it.
    is (1 + 2), 3, 'a narrower core candidate takes the call';
    is (1 + 2.5), 3.5, 'and another core row takes another operand type';
    is (1 + 2), 3, 'and the first answer is unchanged once memoized';
}

{
    multi infix:<->(UInt $a, UInt $b) { "sub:" ~ callsame() }
    # A subset out-narrows the core `(Int:D, Int:D)`, so the user candidate
    # takes the call whenever its predicate binds; a negative operand fails
    # the bind and the core operator answers. Same operand types every time,
    # different winners -- the memo holds the ranking, not the winner.
    is (5 - 3), 'sub:2', 'a subset candidate out-ranks core when it binds';
    is (-5 - 3), -8, 'the same operand types fall back to core when it does not';
    is (7 - 1), 'sub:6', 'and reach the user candidate again afterwards';
}

{
    class P { has $.v }
    multi infix:<+>(P:D $a, P:D $b) { P.new(v => $a.v + $b.v) }
    multi infix:<+>(P:U $a, P:U $b) { "types" }
    is (P.new(v => 1) + P.new(v => 2)).v, 3, 'instances reach the :D candidate';
    is (P + P), 'types', 'type objects key apart from instances and reach the :U one';
    is (1 + 2), 3, 'and Int operands still reach core';
}
