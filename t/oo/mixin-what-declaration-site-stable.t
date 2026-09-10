use Test;

plan 6;

# ADR-0060 gave a role-mixed value's `.WHAT` a genuine per-composition
# identity, keyed by (base type, role name, role id, typeargs). That role id
# must be minted once per *declaration site*, not once per *runtime
# evaluation* of the declaration — otherwise composing the SAME role literal
# twice (e.g. by calling the same sub twice) produces two distinct `.WHAT`s,
# even though Rakudo gives them the same one.

class Foo { has $.x = 1; }

# Composing the same anonymous role literal twice (via two calls to the same
# sub) must produce mixin values that share one `.WHAT`.
sub mk-anon() { return Foo.new but role :: { has $.tag = "hello" }; }
{
    my $o1 = mk-anon();
    my $o2 = mk-anon();
    ok $o1.WHAT === $o2.WHAT,
        'same anon role literal, evaluated twice, shares one .WHAT';
    is $o1.^name, 'Foo+{<anon|1>}', '.^name renders the anon role as <anon|N>';
}

# Two textually DIFFERENT anonymous role literals must NOT share a `.WHAT`,
# even though both mix onto the same base type.
{
    my $o1 = Foo.new but role :: { has $.tag = "a" };
    my $o2 = Foo.new but role :: { has $.tag = "b" };
    nok $o1.WHAT === $o2.WHAT,
        'two different anon role literals do not share .WHAT';
}

# A named role (`my role R {}`) declared inside a repeatedly-called sub must
# also keep one stable identity across calls.
sub mk-named() {
    my role R { has $.tag = "x" }
    return 1 but R;
}
{
    my $o1 = mk-named();
    my $o2 = mk-named();
    ok $o1.WHAT === $o2.WHAT,
        'same named role declaration, evaluated twice, shares one .WHAT';
}

# Two different `my role A {}` declarations that happen to share a short
# name (in different scopes) must still produce distinct mixin identities.
sub mk1() {
    my role A { has $.tag = "x" }
    return 1 but A;
}
sub mk2() {
    my role A { has $.tag = "y" }
    return 1 but A;
}
{
    my $o1 = mk1();
    my $o2 = mk2();
    nok $o1.WHAT === $o2.WHAT,
        'two distinct my role A {} declarations do not share .WHAT';
}

# A role composed via `does` at declaration time keeps its identity too.
{
    role Tagged2 { has $.tag = "t" }
    my $o1 = Foo.new does Tagged2;
    my $o2 = Foo.new does Tagged2;
    ok $o1.WHAT === $o2.WHAT,
        'the same named role composed via does twice shares one .WHAT';
}
