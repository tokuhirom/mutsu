use Test;
use lib 't/lib';

# ADR-0097 §15: a cell or Proxy no longer disables the GetLocal fast path for
# the whole process. The fast path instead relies on an invariant: when a
# frame's env names a container for a variable, the variable's slot holds
# that same container. Each block below is a shape that used to break the
# invariant and leaned on the slow path's read-time adoption to read the
# right value. Debug builds assert the invariant on every fast read, so a
# regression here shows up as a panic in addition to a wrong value.

plan 13;

# A `for` over a scalar aliases the scalar's container; the topic write
# goes through it.
{
    my $a = 1;
    for $a { $_ = 9 }
    is $a, 9, 'for over a scalar: the topic write reaches the variable';
    $a = 3;
    is $a, 3, 'and the variable stays writable afterwards';
}

# ... including from a nested block inside the loop body.
{
    my $x = 'old';
    for $x {
        if True { $_ = 'new' }
    }
    is $x, 'new', 'a topic write from a nested block reaches the variable';
}

# A `:=` made inside a sub on a free variable is spliced into the caller's
# env; the caller's slot has to take the same cell.
{
    my $var = 1;
    my $alias;
    sub bind-it { $alias := $var }
    bind-it();
    $alias = 2;
    is $var, 2, 'a sub-made bind writes through to the caller variable';
}
{
    my $var = 1;
    my $alias;
    sub bind-it-again { $alias := $var }
    bind-it-again();
    $alias = 55;
    is $alias, 55, 'a sibling block with the same names reads its own alias';
}

# A raw parameter of a routine imported through a custom EXPORT binds the
# caller's container.
{
    use ExportHookRawParam <ro bump>;
    my $a = 42;
    ok !ro($a), 'a variable passed to a raw parameter is a container';
    bump($a);
    is $a, 43, 'a write through the raw parameter reaches the caller';
}

# A placeholder named by its bare spelling in a for-modifier.
{
    my $r;
    my &g = { $r = $^b for $b };
    g(4);
    is $r, 4, 'a for-modifier over a bare placeholder name binds it';
}

# EVAL boxing a free variable it writes.
{
    my $n = 1;
    EVAL '$n = 2';
    is $n, 2, 'a write made by EVAL is visible in the caller';
    $n++;
    is $n, 3, 'and the variable keeps working afterwards';
}

# A sigilless bind of a scalar whose name a sibling block also declares.
{
    my $v = 10;
    { my $v = 99; }
    my \x := $v;
    x = 1;
    is $v, 1, 'a bind reaches the visible declaration, not a shadowed one';
}

# A `when` block's value carries its variable's container out through the
# succeed signal; the loop over it aliases that variable's slot.
{
    my $a = 41;
    .++ for do given 1 { when True { $a } };
    is $a, 42, 'a when-block container reaches the for loop over it';
}

# An unrelated `:=` elsewhere does not change what a plain local reads.
{
    my @d = 1, 2;
    my @u := @d;
    my $sum = 0;
    $sum += $_ for 1..10;
    is $sum, 55, 'plain locals read correctly next to an unrelated bind';
}
