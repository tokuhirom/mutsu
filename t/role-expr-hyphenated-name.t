use Test;

# A named `role` expression in term/argument position (e.g.
# `$r.^mixin(role name { ... })`) required the name to start with an
# uppercase letter or `_` -- so a lowercase or kebab-cased role name (legal at
# statement position, and legal in real Raku at expression position too, e.g.
# `Test.rakumod`'s `role is-test-assertion { ... }` argument to `.^mixin`)
# failed to parse there with "Two terms in a row". See
# news/2026-08/test-assertion-trait-is-not-introspectable.md item 5.

plan 4;

sub f() { 1 }
lives-ok { f.^mixin(role is-x { method zz(--> True) { } }) },
    'a lowercase kebab-cased role name parses in expression/argument position';

my $y = 5 but role also-lower { method w(--> True) { } };
ok $y.w, 'a lowercase kebab-cased role name works as a `but` mixin operand too';

# Regression guards: the existing uppercase-name and anonymous forms still work.
my $x = role Named { method zz(--> True) { } };
is $x.^name, 'Named', 'an uppercase named role expression still parses';

my $anon = role { method zz(--> True) { } };
ok $anon.^name.starts-with('<anon'), 'an anonymous role expression still parses';

done-testing;
