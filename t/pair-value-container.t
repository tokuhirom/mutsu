use Test;

plan 15;

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
#
# NOTE: this is a DIVERGENCE, pinned as-is only to record the current
# behaviour. raku dies here -- `my $p = (k => 5); $p.value = 9` raises
# "Cannot modify an immutable Int (5)", because a Pair built from a literal
# has no container behind its value. mutsu fakes the write by rebinding `$p`'s
# own env entry. Removing that fake is ADR-0036 slice 4's remaining half; see
# todo/tickets/pair-value-assign-does-not-enforce-immutable-value.md.
{
    my $p = (k => 5);
    $p.value = 9;
    is $p.value, 9, 'pair built from a literal still allows .value assignment (DIVERGES: raku dies)';
}

# An UNINITIALIZED declared scalar is still a container, so the Pair aliases
# it and `.value = X` writes through -- exactly as it does for an initialized
# one above. mutsu used to capture the bare type object instead, so the write
# had nothing to reach: the Pair printed `k => 5` while `$x` stayed `Int`.
{
    my Int $x;
    my $p = (k => $x);
    $p.value = 5;
    is $x, 5, 'an uninitialized declared scalar is captured as a container';
    is $p.value, 5, 'the pair reads back the value it wrote through';
}
{
    # Untyped, and the write comes back out through a second read of the pair.
    my $y;
    my $p = (k => $y);
    $p.value = "set";
    is $y, "set", 'an uninitialized UNTYPED scalar is captured too';
}
{
    # Boxing each uninitialized scalar into its OWN cell is what keeps distinct
    # variables distinct -- two pairs over two undefined scalars must not share.
    my $a;
    my $b;
    my $pa = (k => $a);
    my $pb = (k => $b);
    $pa.value = 1;
    is $b, Any, 'a sibling undefined scalar is untouched by the other pair';
    $pb.value = 2;
    is $a, 1, 'and the first pair still owns its own container';
    is $b, 2, 'while the second owns its own';
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
