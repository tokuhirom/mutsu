use Test;

# `is_infix_word_op` (src/parser/primary/ident/predicates.rs) lists every
# name that is also an infix operator word (Z X R x xx eq ne lt gt le ge cmp
# coll unicmp leg and or not div mod gcd lcm but does min max ff fff before
# after andthen orelse notandthen), and the identifier parser used to refuse
# treating ANY of them as a paren-less listop call, even when a `sub` of that
# exact name was declared and in scope -- so `before { ... }` parsed as a
# no-arg call to `before()` with the block left dangling, instead of a call
# to `before` with the block as its argument.

plan 6;

{
    my $called = False;
    my $arg-ran = False;
    sub before(&cb) { $called = True; cb(); }
    before { $arg-ran = True };
    ok $called, 'paren-less call to a declared sub named "before" runs it';
    ok $arg-ran, '... with the following block bound as its argument';
}

{
    sub after(&cb) { cb() }
    is after({ 42 }), 42, 'paren-less call to a declared sub named "after" works too';
}

# A declared sub can even be named after a common word-infix operator
# ("eq"/"and"), matching raku.
{
    sub eq(&cb) { cb() }
    is eq({ 43 }), 43, 'a declared sub literally named "eq" is callable paren-less';
}

# Ambiguity guards: on the SAME line, right after a complete term, the word
# must still parse as the infix operator -- never as a second listop call to
# a same-named declared sub.
{
    sub before2(&cb) { cb() }  # (kept distinct from the block below on purpose)
    ok ({ 1 } before { 2 }).WHAT === Bool, 'same-line "before" after a complete term is still the infix';
}
{
    sub and(&cb) { cb() }
    is (1 and 2), 2, '"and" after a complete term is still the infix, even with a same-named declared sub';
}

done-testing;
