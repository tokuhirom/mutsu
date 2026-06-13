use Test;

# A plain positional `@`/`%` parameter binds the caller's container by alias:
# element assignment, `.push`/`splice`, and whole-container `=` assignment all
# propagate to the caller. `is copy` copies; `is raw` already aliased.

plan 15;

# --- array: .push propagates ---
{
    sub a-push(@x) { @x.push(9) }
    my @a = 1, 2, 3;
    a-push(@a);
    is-deeply @a, [1, 2, 3, 9], 'push through plain array param propagates';
}

# --- array: element assignment propagates ---
{
    sub a-elem(@x) { @x[0] = 99 }
    my @b = 1, 2, 3;
    a-elem(@b);
    is-deeply @b, [99, 2, 3], 'element assign through plain array param propagates';
}

# --- array: whole-container assignment propagates ---
{
    sub a-whole(@x) { @x = (7, 8, 9) }
    my @c = 1, 2, 3;
    a-whole(@c);
    is-deeply @c, [7, 8, 9], 'whole-container assign through plain array param propagates';
}

# --- array: splice propagates ---
{
    sub a-splice(@x) { @x.splice(1, 1, 'a', 'b') }
    my @d = 1, 2, 3;
    a-splice(@d);
    is-deeply @d, [1, 'a', 'b', 3], 'splice through plain array param propagates';
}

# --- array: .= mutation propagates ---
{
    sub a-dotmap(@x) { @x .= map(* + 1) }
    my @e = 1, 2, 3;
    a-dotmap(@e);
    is-deeply @e, [2, 3, 4], '.=map through plain array param propagates';
}

# --- array: nested := inside the sub propagates ---
{
    sub a-rebind(@x) { my @y := @x; @y.push(5) }
    my @f = 1, 2, 3;
    a-rebind(@f);
    is-deeply @f, [1, 2, 3, 5], 'transitive := bind inside sub propagates';
}

# --- is copy does NOT propagate ---
{
    sub a-copy(@x is copy) { @x.push(9); @x[0] = 99 }
    my @g = 1, 2, 3;
    a-copy(@g);
    is-deeply @g, [1, 2, 3], 'is copy array param does not propagate';
}

# --- is raw still propagates ---
{
    sub a-raw(@x is raw) { @x.push(9) }
    my @h = 1, 2, 3;
    a-raw(@h);
    is-deeply @h, [1, 2, 3, 9], 'is raw array param propagates';
}

# --- two distinct array params stay independent ---
{
    sub a-two(@x, @y) { @x.push(@y.elems) }
    my @p = 1, 2;
    my @q = 3, 4, 5;
    a-two(@p, @q);
    is-deeply @p, [1, 2, 3], 'first array param mutated';
    is-deeply @q, [3, 4, 5], 'second array param untouched';
}

# --- hash: element assignment propagates ---
{
    sub h-elem(%h) { %h<k> = 5 }
    my %m = a => 1;
    h-elem(%m);
    is-deeply %m, {a => 1, k => 5}, 'element assign through plain hash param propagates';
}

# --- hash: whole-container assignment propagates ---
{
    sub h-whole(%h) { %h = (m => 1, n => 2) }
    my %n = a => 1;
    h-whole(%n);
    is-deeply %n, {m => 1, n => 2}, 'whole-container assign through plain hash param propagates';
}

# --- hash is copy does NOT propagate ---
{
    sub h-copy(%h is copy) { %h<k> = 5 }
    my %o = a => 1;
    h-copy(%o);
    is-deeply %o, {a => 1}, 'is copy hash param does not propagate';
}

# --- array literal arg: container is writable inside (no propagation target) ---
{
    sub a-lit(@x) { @x.push(9); @x[0] = 100 }
    lives-ok { a-lit([1, 2, 3]) }, 'plain array param bound to a literal is writable';
}

# --- methods: array attribute passed to a plain param mutates in place ---
{
    sub take(@x) { @x.push(42) }
    my @r = 10, 20;
    take(@r);
    take(@r);
    is-deeply @r, [10, 20, 42, 42], 'repeated calls accumulate on the caller container';
}
