use Test;

plan 4;

role WithTraitAux {
    has $.v;

    multi sub trait_auxiliary:<is>(WithTraitAux $trait, Any $container; $val) {
        $.v = $val;
        $container does WithTraitAux($val);
    }
}

my $x = 42;
ok $x does WithTraitAux("tag"), "imperative does with role call succeeds";
is $x.v, "tag", "role attribute is initialized from role-call argument";
ok $x does WithTraitAux, "does check is true after imperative composition";

my $y = 7;
lives-ok { $y does WithTraitAux(99) }, "trait_auxiliary signature parses and does not break role body";
