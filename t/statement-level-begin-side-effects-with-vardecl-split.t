use Test;

plan 4;

# `reorder_at_level` (src/runtime/phasers.rs) hoists bare declarations ahead
# of a statement-level BEGIN whenever any VarDecl in the same block has a
# nested BEGIN/CHECK/INIT PhaserExpr (`has_other_phasers`). Two bugs in that
# hoisting were found together:
#
# 1. A bare `my @a;`/`my %h;` (no explicit initializer) parses with a
#    sigil-based default literal (`Literal(Array([]))`), not `Literal(NIL)`.
#    The hoisting split used to test the initializer expression against a NIL
#    literal to decide whether a real initializer existed, which wrongly
#    treated that default literal as "has an initializer" and spliced a
#    spurious `@a = []` reset into the statement list AFTER a hoisted BEGIN
#    had already mutated the array, silently discarding the BEGIN's effect.
# 2. Independently, the hoisted bare declaration's own interim value used a
#    flat `Literal(NIL)` regardless of sigil. Compiling that for an
#    `@`-sigil variable takes the same path as an explicit `@a = Nil`
#    assignment, which itemizes the Nil into a one-element `[(Any)]` array
#    instead of leaving the array genuinely empty.
#
# Both were fixed together; this test pins the observable end-to-end
# behavior. Checked against Rakudo v2026.06.

{
    my $unused = BEGIN 99; # forces the whole block through reorder_at_level
    my @order;
    BEGIN { @order.push('begin') }
    my $i = 20;
    @order.push('after');
    is-deeply @order, ['begin', 'after'],
        'a statement-level BEGIN mutating an array survives a sibling VarDecl-split';
}

{
    my $unused = BEGIN 1;
    my @a;
    is-deeply @a, [], 'a hoisted bare @-sigil VarDecl stays a genuinely empty Array';
}

{
    my $unused = BEGIN 1;
    my %h;
    is-deeply %h, {}, 'a hoisted bare %-sigil VarDecl stays a genuinely empty Hash';
}

{
    my $unused = BEGIN 1;
    my @a = (1, 2, 3);
    my %h = (x => 1);
    is-deeply (@a, %h), ([1, 2, 3], {x => 1}),
        'a real @/% initializer is unaffected by the hoisting fix';
}
