use Test;

# `orwith EXPR -> PARAM { ... }` must accept a pointy parameter with any sigil,
# not just `$`. Regression: `orwith ... -> &edit { ... }` failed to parse
# ("Unexpected block in infix position") because the orwith pointy-block parser
# only recognized `$`-sigil parameters. (Edit::Files dist binds `-> &edit`.)

plan 5;

# with ... orwith chain, &-sigil pointy bound to a Callable
{
    my &f = { 42 };
    my $out;
    with Any { $out = 'with' }
    orwith &f -> &edit { $out = edit() }
    is $out, 42, "with/orwith with &-sigil pointy (callable)";
}

# if ... orwith ... else chain (the shape Edit::Files uses)
{
    my &f = { 7 };
    my $out;
    if False { $out = 'if' }
    orwith &f -> &edit { $out = edit() }
    else { $out = 'else' }
    is $out, 7, "if/orwith/else with &-sigil pointy";
}

# @-sigil pointy binds the topic as an array
{
    my @vals = 1, 2, 3;
    my $out;
    with Nil { }
    orwith @vals -> @a { $out = @a.sum }
    is $out, 6, "orwith with @-sigil pointy";
}

# %-sigil pointy binds the topic as a hash
{
    my %h = a => 1, b => 2;
    my $out;
    with Nil { }
    orwith %h -> %m { $out = %m<a> + %m<b> }
    is $out, 3, "orwith with %-sigil pointy";
}

# $-sigil pointy still works (no regression)
{
    my $out;
    with Nil { }
    orwith 99 -> $v { $out = $v }
    is $out, 99, "orwith with \$-sigil pointy still works";
}
