use Test;

# `given`/`with` with a pointy parameter (`-> @p` / `-> %p` / `-> $p`) aliases
# the topic source, exactly like `given @a { ... }` aliases `@a`. Container
# mutations through the parameter propagate back to the source. `is copy`
# parameters get a fresh copy and do not propagate.

plan 28;

# --- given: whole-container array param ---
{
    my @a = 1, 2, 3;
    given @a -> @p { @p.push(4) }
    is-deeply @a, [1, 2, 3, 4], 'given @a -> @p { .push }';
}
{
    my @a = 1, 2, 3;
    given @a -> @p { @p[0] = 99 }
    is-deeply @a, [99, 2, 3], 'given @a -> @p { @p[0] = v }';
}
{
    my @a = 1, 2, 3;
    given @a -> @p { @p = (7, 8, 9) }
    is-deeply @a, [7, 8, 9], 'given @a -> @p { @p = (...) } (reassign propagates)';
}

# --- given: hash param ---
{
    my %h = a => 1;
    given %h -> %q { %q<b> = 2 }
    is-deeply %h, {a => 1, b => 2}, 'given %h -> %q { %q<k> = v }';
}

# --- given: scalar param binding a container ---
{
    my @a = 1, 2, 3;
    given @a -> $p { $p.push(4) }
    is-deeply @a, [1, 2, 3, 4], 'given @a -> $p { $p.push }';
}

# --- with: whole-container ---
{
    my @a = 1, 2, 3;
    with @a -> @p { @p.push(4) }
    is-deeply @a, [1, 2, 3, 4], 'with @a -> @p { .push }';
}
{
    my @a = 1, 2, 3;
    with @a -> @p { @p[0] = 99 }
    is-deeply @a, [99, 2, 3], 'with @a -> @p { @p[0] = v }';
}
{
    my %h = a => 1;
    with %h -> %q { %q<b> = 2 }
    is-deeply %h, {a => 1, b => 2}, 'with %h -> %q { %q<k> = v }';
}
{
    my @a = 1, 2, 3;
    with @a -> $p { $p.push(8) }
    is-deeply @a, [1, 2, 3, 8], 'with @a -> $p { $p.push }';
}

# --- element-source topics ---
{
    my %e = k => [1, 2, 3];
    with %e<k> -> @p { @p.push(4) }
    is-deeply %e<k>, [1, 2, 3, 4], 'with %e<k> -> @p { .push }';
}
{
    my @f = [1, 2], [3, 4];
    given @f[1] -> @p { @p.push(9) }
    is-deeply @f, [[1, 2], [3, 4, 9]], 'given @a[i] -> @p { .push }';
}
{
    my %g = k => [1, 2];
    given %g<k> -> $p { $p.push(5) }
    is-deeply %g<k>, [1, 2, 5], 'given %h<k> -> $p { $p.push }';
}

# --- is copy: fresh flattened copy, no propagation ---
{
    my @c = 1, 2, 3;
    my @inner;
    given @c -> @p is copy { @p.push(4); @inner = @p }
    is-deeply @inner, [1, 2, 3, 4], 'given @c -> @p is copy: inner is a flattened copy';
    is-deeply @c, [1, 2, 3], 'given @c -> @p is copy { .push } (no propagation)';
}
{
    my @c = 1, 2, 3;
    my @inner;
    with @c -> @p is copy { @p.push(4); @inner = @p }
    is-deeply @inner, [1, 2, 3, 4], 'with @c -> @p is copy: inner is a flattened copy';
    is-deeply @c, [1, 2, 3], 'with @c -> @p is copy { .push } (no propagation)';
}

# --- when / default inside a pointy given ---
{
    my @w = 1, 2, 3;
    given @w -> @p { when * { @p.push(7) } }
    is-deeply @w, [1, 2, 3, 7], 'given @a -> @p { when * { .push } }';
}
{
    my @w = 1, 2, 3;
    given @w -> @p { default { @p.push(7) } }
    is-deeply @w, [1, 2, 3, 7], 'given @a -> @p { default { .push } }';
}

# --- nested pointy with ---
{
    my @big = 1, 2, 3;
    with @big -> @outer { with @outer -> @inner { @inner.push(99) } }
    is-deeply @big, [1, 2, 3, 99], 'nested with -> @p propagates through both levels';
}

# --- sequential reuse of the same parameter name (no leak) ---
{
    my @a = 1, 2, 3;
    with @a -> @p { @p.push(4) }
    my @b = 1, 2, 3;
    given @b -> @p { @p[0] = 9 }
    is-deeply @a, [1, 2, 3, 4], 'sequential reuse: first block intact';
    is-deeply @b, [9, 2, 3], 'sequential reuse: second block intact';
}

# --- aliasing then is copy on the same name (no stale-alias corruption) ---
{
    my @a = 1, 2, 3;
    with @a -> @p { @p.push(4) }
    my @c = 1, 2, 3;
    given @c -> @p is copy { @p.push(4) }
    is-deeply @a, [1, 2, 3, 4], 'alias-then-copy: aliased source intact';
    is-deeply @c, [1, 2, 3], 'alias-then-copy: copy source unchanged';
}

# --- element-source given followed by whole-container given (element_source not leaked) ---
{
    my %g = k => [1, 2];
    given %g<k> -> @p { @p.push(3) }
    my @w = 1, 2, 3;
    given @w -> @p { @p.push(4) }
    is-deeply %g<k>, [1, 2, 3], 'element-then-whole: element source intact';
    is-deeply @w, [1, 2, 3, 4], 'element-then-whole: whole-container propagates';
}

# --- value/topic still usable; read-only loops are no-ops ---
{
    my @a = 1, 2, 3;
    my $sum = 0;
    given @a -> @p { $sum += $_ for @p }
    is $sum, 6, 'read-only pointy given does not corrupt the source';
    is-deeply @a, [1, 2, 3], 'read-only pointy given leaves the source unchanged';
}

# --- without: false branch never runs the alias body ---
{
    my $ran = False;
    without 42 -> $p { $ran = True }
    is $ran, False, 'without with defined value does not run the body';
}
