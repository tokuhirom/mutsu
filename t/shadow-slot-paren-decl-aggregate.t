use v6;
use Test;

# §1.3 S17 pin (docs/lexical-scope-slot-campaign.md): a parenthesized
# aggregate declaration lvalue (`(my @x) = ...` / `(my %h) = ...`) must stay
# on the by-name in-place assignment even under MUTSU_SHADOW_SLOTS=1.
# `AssignExprLocal` REPLACES the slot for `@`/`%` targets, which detaches the
# container the RHS captured — breaking the circular `.raku.EVAL` roundtrip
# shape `((my @y) = [42, @y])` (roast/S32-array/perl.t #7). Both toggle modes
# must pass this file.

plan 7;

{
    my @a;
    @a = 42, @a;
    is @a.raku.EVAL[1][1][1][0], 42,
        'circular array roundtrips through .raku.EVAL';
}

{
    my $r = ((my @x) = [42, @x]);
    is $r[0], 42, 'paren-decl assignment returns the assigned values';
    is $r[1][1][1][0], 42,
        'RHS self-reference aliases the declared container (cycle intact)';
}

my @x = 9, 9;
my %h = a => 9;
{
    (my @x) = 1, 2;
    is-deeply @x, [1, 2], 'inner paren-decl array shadow sees its own values';
    (my %h) = b => 1;
    is %h<b>, 1, 'inner paren-decl hash shadow sees its own values';
}
is-deeply @x, [9, 9], 'outer array is untouched after the shadow block';
is-deeply %h, {a => 9}, 'outer hash is untouched after the shadow block';
