use Test;

# Longest-token matching (Raku LTM) for user-declared *symbol* infix operators.
# A custom operator whose symbol is longer than — and starts with — a built-in
# operator must win: `sub infix:<+-*/>` makes `5 +-*/ 2` one operator, not
# `5 + (-*/2)` (which would treat `*` as a Whatever).

plan 5;

{
    sub infix:<+-*/>($a, $b) {
        ( { $a + $b }, { $a - $b }, { $a * $b }, { $a / $b } )>>.()
    }
    is-deeply (5 +-*/ 2), (7, 3, 10, 2.5), 'infix:<+-*/> parses as one operator';
}

# A custom operator starting with the built-in `-` (additive level).
{
    sub infix:<-=->($a, $b) { $a * 100 + $b }
    is (7 -=- 3), 703, 'infix:<-=-> beats built-in -';
}

# Built-in operators still work when no custom operator shadows them.
is (5 + 2), 7, 'built-in + still works';
is (5 - 2), 3, 'built-in - still works';
is (5 * 2), 10, 'built-in * still works';
