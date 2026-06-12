use Test;

plan 9;

# `key => $var` captures $var's container, so the Pair's value aliases the
# variable: assigning `.value` writes through to the source variable, and the
# variable's writes are visible through the Pair. (S02:1704)

# Var -> Pair: writing the variable is visible through the pair.
{
    my $v = 1;
    my $p = (k => $v);
    $v = 99;
    is $p.value, 99, 'variable write is visible through the pair value';
}

# Pair -> Var: assigning .value writes through to the variable.
{
    my $v = 1;
    my $p = (k => $v);
    $p.value = 42;
    is $v, 42, 'assigning .value writes through to the source variable';
    is $p.value, 42, 'pair value reflects the assignment';
}

# The :$var colonpair shorthand captures the container the same way.
{
    my $val = "before";
    my $p = (:$val);
    $p.value = "after";
    is $val, "after", ':$var colonpair captures the container';
}

# Reading does not lose the type or value.
{
    my $v = 1;
    my $p = (k => $v);
    isa-ok $p.value, Int, 'pair value keeps its type';
    is $p.raku, ':k(1)', 'pair value renders correctly';
}

# A literal value is not a container; assignment still updates the pair.
{
    my $p = (k => 5);
    $p.value = 9;
    is $p.value, 9, 'pair built from a literal still allows .value assignment';
}

# Storing a captured pair into a Hash decontainerizes the value (Raku copies
# the value into the hash slot, so a later write to the source is not seen).
{
    my $v = 1;
    my %h = (k => $v);
    $v = 2;
    is %h<k>, 1, 'hash store decontainerizes the pair value';
}

# A `key => $var` named argument does not leak a container into the callee
# (the value binds/stores as a plain value).
{
    class C { has $.x }
    my $dir = "hello";
    my $c = C.new(x => $dir);
    is $c.x, "hello", 'named-argument pair value reaches attribute as a plain value';
}
