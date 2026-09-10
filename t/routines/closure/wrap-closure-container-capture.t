use Test;

plan 6;

# A wrapper closure that mutates a captured outer container must have that
# mutation visible to the caller after the wrapped call returns. Previously a
# container mutated ONLY via element-assign (`%h{$k} = v`) was not captured as a
# free variable, so a wrapper's hash element-assign was lost on return (its
# copy-on-write result was neither shared with the caller nor persisted for the
# closure writeback). Scalars, array element-assign (shared in place) and
# `.push` already worked; this covers the hash element-assign gap.

# Hash element-assign inside a wrapper.
{
    my %h;
    sub f($x) { 42 }
    &f.wrap(-> $x { %h{$x} = 99; callwith($x) });
    f(5);
    is-deeply %h, {5 => 99}, 'hash element-assign in a wrapper is visible to the caller';
}

# Accumulating across several calls.
{
    my %seen;
    sub g($x) { $x * 2 }
    &g.wrap(-> $x { %seen{$x} = True; callwith($x) });
    g(1); g(2); g(3);
    is-deeply %seen, {1 => True, 2 => True, 3 => True}, 'hash accumulates across wrapped calls';
}

# Array element-assign (already worked, keep as a guard).
{
    my @a = 0, 0, 0;
    sub h($x) { 7 }
    &h.wrap(-> $x { @a[$x] = 99; callwith($x) });
    h(1);
    is-deeply @a, [0, 99, 0], 'array element-assign in a wrapper is visible to the caller';
}

# Scalar assign (already worked, keep as a guard).
{
    my $s = 0;
    sub k($x) { 1 }
    &k.wrap(-> $x { $s = 99; callwith($x) });
    k(5);
    is $s, 99, 'scalar assign in a wrapper is visible to the caller';
}

# Hash .push inside a wrapper (already worked, keep as a guard).
{
    my %m;
    sub p($x) { 1 }
    &p.wrap(-> $x { %m.push($x => 1); callwith($x) });
    p(5);
    is-deeply %m, {5 => 1}, 'hash .push in a wrapper is visible to the caller';
}

# The wrapped call still returns the correct value.
{
    my %h;
    sub q($x) { $x + 100 }
    &q.wrap(-> $x { %h{$x} = 1; callwith($x) });
    is q(5), 105, 'wrapped call returns the original result';
}
