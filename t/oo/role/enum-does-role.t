use Test;

plan 5;

# Basic role mixin on enum value
{
    my enum Color <Red Green Blue>;
    my $x = Red does role { method name() { "red" } };
    is $x.name, "red", "role mixin on enum value (via variable)";
}

# BareWord does role (modifies in place)
{
    my enum Fruit <Apple Banana>;
    Apple does role { method taste() { "sweet" } };
    is Apple.taste, "sweet", "BareWord enum does role modifies in place";
}

# Private method in role mixin on enum
{
    my enum Animal <Cat Dog>;
    Cat does role {
        method !secret { 42 }
        method reveal() { self!secret }
    };
    is Cat.reveal, 42, "private method works in role mixin on enum";
}

# Role with attribute on enum value
{
    my enum Dir <Up Down>;
    my $d = Up does role { has $.label = "upward" };
    is $d.label, "upward", "role attribute on enum value";
}

# Enum value retains enum behavior after mixin
{
    my enum Size <Small Medium Large>;
    Small does role { method desc() { "tiny" } };
    is Small.desc, "tiny", "enum with role mixin works";
}
