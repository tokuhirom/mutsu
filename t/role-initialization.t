use Test;

plan 9;

# X::Role::Initialization: supplying an initialization value to a role that
# does not have exactly one public attribute is illegal.
throws-like 'my role R { }; 99 but R("wrong")', X::Role::Initialization;
throws-like 'my role R { has $.x; has $.y }; 99 but R("wrong")', X::Role::Initialization;
throws-like 'my role R { }; 99 does R("wrong")', X::Role::Initialization;
throws-like 'my role R { has $.x; has $.y }; 99 does R("wrong")', X::Role::Initialization;

# A role with a single public attribute accepts a positional init value.
{
    my role R { has $.x }
    my $o = 99 but R("ok");
    is $o.x, "ok", 'but R(value) initializes the single public attribute';
}
{
    my role R { has $.x }
    my $o = 99 does R("yo");
    is $o.x, "yo", 'does R(value) initializes the single public attribute';
}

# Plain `but Role` (no init value) must still compose without error.
{
    my role R { method greet { "hi" } }
    my $o = 99 but R;
    is $o.greet, "hi", 'but Role with no init value still works';
}

# Standalone `Role(value)` (outside but/does) still coerces and errors.
throws-like 'my role R { has $.x }; my $v = R("x")', Exception;

# Mixing a concrete role with a method still dispatches.
{
    my role Named { has $.name }
    my $p = 42 but Named("Ann");
    is $p.name, "Ann", 'named single-attr role init';
}
