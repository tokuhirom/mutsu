use Test;

# §1.4 shadow-slot leaf: `undefine($x)` on a variable that shadows an enclosing
# same-name binding must clear the inner (live) binding, not the outer one. The
# undefine rewrite now prefers the compile-time-baked local slot
# (AssignExprLocal) like the general assignment path. Passes with the shadow-slot
# gate off (shadow shares the outer slot) AND on (distinct slots). Mirrors
# roast/S32-scalar/defined.t #31. See docs/lexical-scope-slot-campaign.md.

plan 4;

my $foo = 42;
{
    my $foo = "inner";
    undefine($foo);
    ok !$foo.defined, 'undefine clears the inner shadow';
}
ok $foo.defined, 'outer binding untouched by inner undefine';
is $foo, 42, 'outer value intact';

# undefine on a non-shadowed variable still works.
my $bar = 7;
undefine($bar);
ok !$bar.defined, 'undefine on a plain (non-shadowed) scalar';
