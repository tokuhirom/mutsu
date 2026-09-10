use Test;

# A `for ... -> $var` loop binds a *fresh* `$var` per iteration, so a closure
# created in the body and called *after* the loop must see that iteration's
# value, not the loop's final value. Previously the int-range fast path
# (`exec_for_loop_int_range`) never saved/restored the named param, so `$var`
# leaked past the loop with its final value; the leaked value then shadowed each
# closure's correctly-frozen captured value (the capture merge does not overwrite
# an existing call-site binding), making every closure read the final value.
# See docs/vm-dual-store.md / PLAN.md lever C (closure capture).

plan 14;

# Stored closures over the loop variable, called after the loop.
{
    my @c;
    for 1..3 -> $i {
        @c.push({ $i });
    }
    is @c[0](), 1, 'closure 0 captured its iteration value';
    is @c[1](), 2, 'closure 1 captured its iteration value';
    is @c[2](), 3, 'closure 2 captured its iteration value';
}

# Pointy-block closures over the loop variable.
{
    my @d;
    for 1..3 -> $j {
        @d.push(-> { $j * 10 });
    }
    is @d[0](), 10, 'pointy closure 0';
    is @d[1](), 20, 'pointy closure 1';
    is @d[2](), 30, 'pointy closure 2';
}

# Nested closure capturing the loop variable through two levels.
{
    my @e;
    for 1..3 -> $i {
        @e.push(-> { -> { $i } });
    }
    is @e[0]()(), 1, 'nested closure 0';
    is @e[2]()(), 3, 'nested closure 2';
}

# Called inside the loop still sees the current iteration value.
{
    my @seen;
    for 1..3 -> $i {
        my $c = { $i };
        @seen.push($c());
    }
    is @seen.join(','), '1,2,3', 'closure called inside loop sees current value';
}

# The loop variable must not leak past the loop into the enclosing scope.
{
    my $leaked = 'unset';
    for 1..3 -> $i { }
    $leaked = (try { $i } // 'gone');
    is $leaked, 'gone', 'loop variable does not leak past the loop';
}

# LAST/post phasers still observe the loop variable at its final value.
{
    my $last-seen;
    for 1..3 -> $i {
        LAST { $last-seen = $i }
    }
    is $last-seen, 3, 'LAST phaser sees final loop-variable value';
}

# Nested loops reusing the same parameter name restore the outer binding.
{
    my @out;
    for 1..2 -> $x {
        for 10, 20 -> $x { }
        @out.push($x);
    }
    is @out.join(','), '1,2', 'inner loop param does not clobber outer same-named param';
}

# A non-range (list) for-loop already restored its param; keep it pinned.
{
    my @c;
    for <a b c> -> $s {
        @c.push({ $s });
    }
    is @c[0](), 'a', 'list-loop closure 0';
    is @c[2](), 'c', 'list-loop closure 2';
}
