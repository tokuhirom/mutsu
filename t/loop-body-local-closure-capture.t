use Test;

# A `my` declared in a loop *body* is a fresh binding each iteration, so a
# closure created in the body and called later must see its iteration's value,
# not the loop's final value. Previously every such closure read the loop's last
# value (e.g. `333` instead of `123`): the closure froze the correct value in its
# captured env (COW), but read the free variable lazily by name from the caller
# env, where the dual-store `sync_env_from_locals` re-injected the body-local's
# slot value (the final iteration's). This is fixed by marking loop-body-declared
# free variables as the closure's `owned_captures` (VM::loop_local_vars): at call
# time they are read from the closure's own frozen captured env, immune to the
# caller-env re-injection. See PLAN.md lever C (ContainerRef upvalue, slice 1).
#
# NOTE: intra-iteration mutation *after* capture
# (`for ... { my $x = 1; my $c = {$x}; $x = 2; ... $c() }` should see 2) is a
# known follow-up requiring a shared ContainerRef cell; this slice freezes the
# value at capture, which is correct for the common read-only / cross-iteration
# case below.

plan 15;

# for over an integer range
{
    my @c;
    for 1..3 { my $x = $_; @c.push({ $x }) }
    is @c[0](), 1, 'for-range body-local closure 0';
    is @c[1](), 2, 'for-range body-local closure 1';
    is @c[2](), 3, 'for-range body-local closure 2';
}

# for over a list
{
    my @c;
    for <a b c> { my $s = $_; @c.push({ $s }) }
    is @c.map(*.()).join(','), 'a,b,c', 'for-list body-local closures';
}

# for with a named param, body-local derived value
{
    my @c;
    for 1..3 -> $v { my $y = $v * 2; @c.push({ $y }) }
    is @c.map(*.()).join(','), '2,4,6', 'for -> body-local derived closures';
}

# while loop
{
    my @c;
    my $k = 0;
    while $k < 3 { my $z = $k; @c.push({ $z }); $k++ }
    is @c.map(*.()).join(','), '0,1,2', 'while body-local closures';
}

# C-style loop
{
    my @c;
    loop (my $i = 0; $i < 3; $i++) { my $w = $i; @c.push({ $w }) }
    is @c.map(*.()).join(','), '0,1,2', 'C-style body-local closures';
}

# repeat loop
{
    my @c;
    my $m = 0;
    repeat { my $p = $m; @c.push({ $p }); $m++ } while $m < 3;
    is @c.map(*.()).join(','), '0,1,2', 'repeat body-local closures';
}

# an intervening call between loop and closure invocation must not corrupt it
{
    my @c;
    for 1..3 { my $x = $_; @c.push({ $x }) }
    my $unrelated = (1..10).sum;
    is @c[0]() + @c[1]() + @c[2](), 6, 'closures survive an intervening call';
}

# each iteration's closure keeps its own mutable state (mutating closure)
{
    my @c;
    for 1..2 { my $x = $_; @c.push({ $x++; $x }) }
    is @c[0](), 2, 'mutating loop closure 0 first call';
    is @c[0](), 3, 'mutating loop closure 0 second call (own state)';
    is @c[1](), 3, 'mutating loop closure 1 independent (own $x = 2, ++ -> 3)';
}

# a closure over an enclosing (non-loop) lexical still late-binds (shared binding)
{
    my $g = 1;
    my $cl = { $g };
    $g = 9;
    is $cl(), 9, 'non-loop closure still sees live shared binding';
}

# a loop that mutates an enclosing variable still works (not owned)
{
    my $sum = 0;
    for 1..5 { $sum += $_ }
    is $sum, 15, 'loop mutation of enclosing var propagates';
}

# nested closures over the loop variable through two levels
{
    my @c;
    for 1..3 -> $i { @c.push(-> { -> { $i } }) }
    is @c.map({ .().() }).join(','), '1,2,3', 'nested closure over loop var';
}
