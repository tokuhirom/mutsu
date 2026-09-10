use Test;

# Regression: a `for ^N { ... }` / `for 0..N { ... }` loop (the int-range fast
# path) marks its implicit topic `$_` readonly for the loop body. A `$x ~~ s///`
# inside the body topicalizes `$x` — `$_` is *aliased* to `$x` for the match — so
# the substitution's readonly check must reflect `$x`'s mutability, not the
# loop topic's. Previously the stale readonly mark on `_` made the s/// throw
# "Cannot modify an immutable Str" even though `$x` is a mutable `my` variable.

plan 9;

# 1. the core case: explicit mutable LHS in an int-range loop
{
    my $x = "0000";
    for ^2 { $x ~~ s/0/1/; }
    is $x, "1100", '$x ~~ s/// inside for ^N mutates the mutable $x';
}

# 2. single iteration
{
    my $x = "00";
    for ^1 { $x ~~ s/0/1/; }
    is $x, "10", 'for ^1 { $x ~~ s/// } works';
}

# 3. explicit range form
{
    my $x = "aaaa";
    for 0..2 { $x ~~ s/a/b/; }   # 0,1,2 = 3 iterations, one leftmost `a` each
    is $x, "bbba", 'for 0..N { $x ~~ s/// } mutates $x once per iteration (first match)';
}

# 4. global substitution per iteration
{
    my $x = "aaaa";
    for ^2 { $x ~~ s:g/a/b/; }
    is $x, "bbbb", 'for ^N { $x ~~ s:g/// } works';
}

# 5. nested int-range loops
{
    my $x = "0000";
    for ^2 { for ^2 { $x ~~ s/0/9/; } }
    is $x, "9999", 'nested for ^N loops mutate $x';
}

# 6. tr/// with explicit var in an int-range loop (was already OK; guard it)
{
    my $x = "aaa";
    for ^2 { $x ~~ tr/a/b/; }
    is $x, "bbb", 'for ^N { $x ~~ tr/// } works';
}

# 7. interleave with an unrelated hot local
{
    my $x = "0000";
    my $acc = 0;
    for ^3 { $x ~~ s/0/1/; $acc++; }
    is "$x/$acc", "1110/3", 's/// + hot local in for ^N both correct';
}

# 8. the implicit topic is STILL readonly: `s///` on the loop topic must throw
{
    dies-ok { for ^2 { s/0/1/; } }, 'bare s/// on the readonly int-range topic still dies';
}

# 9. explicit `$_ ~~ s///` on the loop topic must also still throw
{
    dies-ok { for ^2 { $_ ~~ s/0/1/; } }, '$_ ~~ s/// on the readonly int-range topic still dies';
}
